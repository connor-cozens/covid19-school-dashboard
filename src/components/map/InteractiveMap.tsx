'use client';

import { useCallback, useEffect, useRef, useMemo, useState } from 'react';
import { MapContainer, TileLayer, CircleMarker, ZoomControl, useMap } from 'react-leaflet';
import { useDashboardStore } from '@/store/dashboard-store';
import { useDemographicsLookup } from '@/hooks/useDemographicsLookup';
import { SchoolCase, SchoolClosure } from '@/types';
import { getClosureDate, getReopeningDate, getClosureBoardName, getClosureSchoolName } from '@/lib/closure-fields';
import { ONTARIO_BOUNDS, MAP_DEFAULT_CENTER, MAP_DEFAULT_ZOOM, MAP_MIN_ZOOM, MAP_MAX_ZOOM, CASE_COLOR_LOW, CASE_COLOR_HIGH, CASE_COLOR_CLOSURE } from '@/lib/constants';
import { aggregateCases, getCaseMarkerColor, getCaseMarkerRadius, AggregatedSchoolCase } from '@/lib/map-aggregation';
import MapFullscreenButton from './MapFullscreenButton';
import LazyPopup from './LazyPopup';
import 'leaflet/dist/leaflet.css';

// Fix for default markers in React-Leaflet
import L from 'leaflet';
delete (L.Icon.Default.prototype as unknown as { _getIconUrl?: string })._getIconUrl;
L.Icon.Default.mergeOptions({
  iconRetinaUrl: 'https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.7.1/images/marker-icon-2x.png',
  iconUrl: 'https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.7.1/images/marker-icon.png',
  shadowUrl: 'https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.7.1/images/marker-shadow.png',
});

interface InteractiveMapProps {
  className?: string;
  isFullscreen?: boolean;
}

// Move MapEventHandler outside of the main component to prevent recreation on every render
function MapEventHandler({ onBoundsChange }: { onBoundsChange: (bounds: L.LatLngBounds) => void }) {
  const map = useMap();
  // Track the last reported bbox so re-renders triggered by marker changes
  // (which cause Leaflet to re-fire moveend via _onPanTransitionEnd) don't
  // loop: if the viewport hasn't actually changed we skip the setState call.
  const prevBboxKeyRef = useRef('');

  useEffect(() => {
    function closePopupOnMove() {
      map.closePopup();
    }

    function update() {
      const b = map.getBounds();
      const key = [b.getNorth(), b.getSouth(), b.getEast(), b.getWest()]
        .map(n => n.toFixed(4)).join(',');
      if (key === prevBboxKeyRef.current) return;
      prevBboxKeyRef.current = key;
      onBoundsChange(b);
    }
    map.on('movestart', closePopupOnMove);
    map.on('moveend', update);
    map.on('zoomend', update);
    update();
    return () => {
      map.off('movestart', closePopupOnMove);
      map.off('moveend', update);
      map.off('zoomend', update);
    };
  }, [map, onBoundsChange]);

  return null;
}

export default function InteractiveMap({ className, isFullscreen }: InteractiveMapProps) {
  const { filteredCases, filteredClosures, showDemographics, isMapFullscreen } = useDashboardStore();
  const { demographicsLookup, getDemographics } = useDemographicsLookup();
  const [mapBounds, setMapBounds] = useState<L.LatLngBounds | null>(null);
  const handleBoundsChange = useCallback((bounds: L.LatLngBounds) => setMapBounds(bounds), []);

  // Aggregate cases per school — one marker per unique school (by school_number, fallback to school_name).
  // demographicsLookup is a dependency so markers update once the lookup finishes loading,
  // avoiding the race condition where the popup opens before the async fetch completes.
  const aggregatedCases = useMemo(() => {
    return aggregateCases(filteredCases, demographicsLookup);
  }, [filteredCases, demographicsLookup]);

  // Viewport-culled views — only render markers within the current map bounds.
  // Using a small pad (0.1 = 10%) to pre-load markers just outside the visible area
  // so they don't pop in abruptly during slow pans.
  const visibleCases = useMemo(() => {
    if (!mapBounds) return aggregatedCases;
    const padded = mapBounds.pad(0.1);
    return aggregatedCases.filter(s => padded.contains([s.latitude as number, s.longitude as number]));
  }, [aggregatedCases, mapBounds]);

  const visibleClosures = useMemo(() => {
    if (!mapBounds) return filteredClosures;
    const padded = mapBounds.pad(0.1);
    return filteredClosures.filter(
      c => c.latitude && c.longitude && padded.contains([c.latitude as number, c.longitude as number])
    );
  }, [filteredClosures, mapBounds]);

  // Helper function to render case markers
  const renderCaseMarker = (schoolCase: AggregatedSchoolCase) => {
    if (!schoolCase.latitude || !schoolCase.longitude) {
      return null; // Skip markers with missing coordinates
    }

    const totalCases = schoolCase.total_confirmed_cases || 0;

    // Determine marker color based on case count — matches R app thresholds
    const color = getCaseMarkerColor(totalCases);

    // Determine marker size based on case count
    const radius = getCaseMarkerRadius(totalCases);

    const municipality = schoolCase.municipality || (schoolCase as SchoolCase & { city?: string }).city || '';

    return (
      <CircleMarker
        key={`case-${schoolCase.school_number || schoolCase.school_name}`}
        center={[schoolCase.latitude, schoolCase.longitude]}
        radius={radius}
        pathOptions={{
          color: color,
          fillColor: color,
          fillOpacity: 0.7,
          weight: 2,
        }}
      >
        <LazyPopup>
          <div className="p-2 max-h-64 overflow-y-auto">
            <h3 className="font-semibold text-lg">{schoolCase.school_name}</h3>
            <p className="text-sm text-gray-600">{schoolCase.school_board}</p>
            {municipality && <p className="text-sm text-gray-600">{municipality}</p>}
            <div className="mt-2 space-y-1">
              <p className="text-sm">
                <span className="font-medium">Total Cases:</span> {schoolCase.total_confirmed_cases}
              </p>
              <p className="text-sm">
                <span className="font-medium">Student Cases:</span> {schoolCase.confirmed_student_cases}
              </p>
              <p className="text-sm">
                <span className="font-medium">Staff Cases:</span> {schoolCase.confirmed_staff_cases}
              </p>
              <p className="text-sm">
                <span className="font-medium">Unspecified Cases:</span> {schoolCase.confirmed_unspecified_cases}
              </p>
            </div>
            <p className="text-xs text-gray-500 mt-2">
              Latest report: {schoolCase.collected_date ? new Date(schoolCase.collected_date).toLocaleDateString() : 'N/A'}
            </p>
            {schoolCase._recordCount > 1 && (
              <p className="text-xs text-gray-400 mt-1">
                ({schoolCase._recordCount} daily reports aggregated)
              </p>
            )}
            {showDemographics && schoolCase._demo && (
              <div className="mt-2 pt-2 border-t border-gray-200">
                <h4 className="font-medium text-sm mb-1">School Info:</h4>
                <p className="text-xs">{schoolCase._demo.school_level} · {schoolCase._demo.school_language} · Gr. {schoolCase._demo.grade_range}</p>
                <p className="text-xs">Enrolment: {schoolCase._demo.enrolment ?? 'N/A'}</p>
                {schoolCase._demo.demographics && (
                  <>
                    <h4 className="font-medium text-sm mb-1 mt-1">Demographics:</h4>
                    <p className="text-xs">Low-income households: {schoolCase._demo.demographics.percentage_of_school_aged_children_who_live_in_low_income_households ?? 'N/A'}%</p>
                    {schoolCase._demo.school_language === 'French' ? (
                      <>
                        <p className="text-xs">First language not French: {schoolCase._demo.demographics.percentage_of_students_whose_first_language_is_not_french ?? 'N/A'}%</p>
                        <p className="text-xs">New to Canada (non-FR): {schoolCase._demo.demographics.percentage_of_students_who_are_new_to_canada_from_a_non_french_speaking_country ?? 'N/A'}%</p>
                      </>
                    ) : (
                      <>
                        <p className="text-xs">First language not English: {schoolCase._demo.demographics.percentage_of_students_whose_first_language_is_not_english ?? 'N/A'}%</p>
                        <p className="text-xs">New to Canada (non-EN): {schoolCase._demo.demographics.percentage_of_students_who_are_new_to_canada_from_a_non_english_speaking_country ?? 'N/A'}%</p>
                      </>
                    )}
                    <p className="text-xs">Special education: {schoolCase._demo.demographics.percentage_of_students_receiving_special_education_services ?? 'N/A'}%</p>
                    <p className="text-xs">Gifted: {schoolCase._demo.demographics.percentage_of_students_identified_as_gifted ?? 'N/A'}%</p>
                    <p className="text-xs">Parents w/o degree: {schoolCase._demo.demographics.percentage_of_students_whose_parents_have_no_degree__diploma_or_certificate ?? 'N/A'}%</p>
                  </>
                )}
              </div>
            )}
          </div>
        </LazyPopup>
      </CircleMarker>
    );
  };

  // Helper function to render closure markers
  const renderClosureMarker = (closure: SchoolClosure) => {
    if (!closure.latitude || !closure.longitude) {
      return null; // Skip markers with missing coordinates
    }

    const schoolName = getClosureSchoolName(closure) || 'Unknown School';
    const boardName = getClosureBoardName(closure) || 'Unknown Board';
    const closureDate = getClosureDate(closure);
    const reopeningDate = getReopeningDate(closure);
    const reason = closure['Reason for Closure'] || closure.reason_for_closure || 'N/A';
    // Look up school info and demographics from the lookup — closure records only store lat/lng
    const demo = getDemographics(closure.school_number as number);

    return (
      <CircleMarker
        key={`closure-${schoolName || 'unknown'}-${closureDate || 'no-date'}`}
        center={[closure.latitude, closure.longitude]}
        radius={12}
        pathOptions={{
          color: CASE_COLOR_CLOSURE,
          fillColor: CASE_COLOR_CLOSURE,
          fillOpacity: 0.7,
          weight: 2,
        }}
      >
        <LazyPopup>
          <div className="p-2 max-h-64 overflow-y-auto">
            <h3 className="font-semibold text-lg">{schoolName || 'Unknown School'}</h3>
            <p className="text-sm text-gray-600">{boardName || 'Unknown Board'}</p>
            <p className="text-sm text-gray-600">{closure.city || ''}</p>
            <div className="mt-2 space-y-1">
              <p className="text-sm">
                <span className="font-medium">Closure Date:</span> {closureDate ? new Date(closureDate).toLocaleDateString() : 'N/A'}
              </p>
              <p className="text-sm">
                <span className="font-medium">Reopening Date:</span> {reopeningDate ? new Date(reopeningDate).toLocaleDateString() : 'N/A'}
              </p>
              <p className="text-sm">
                <span className="font-medium">Reason:</span> {reason || 'N/A'}
              </p>
            </div>
            {showDemographics && demo && (
              <div className="mt-2 pt-2 border-t border-gray-200">
                <h4 className="font-medium text-sm mb-1">School Info:</h4>
                <p className="text-xs">{demo.school_level} · {demo.school_language} · Gr. {demo.grade_range}</p>
                <p className="text-xs">Enrolment: {demo.enrolment != null ? demo.enrolment.toLocaleString() : 'N/A'}</p>
                {demo.demographics && (
                  <>
                    <h4 className="font-medium text-sm mb-1 mt-1">Demographics:</h4>
                    <p className="text-xs">Low-income households: {demo.demographics.percentage_of_school_aged_children_who_live_in_low_income_households ?? 'N/A'}%</p>
                    {demo.school_language === 'French' ? (
                      <>
                        <p className="text-xs">First language not French: {demo.demographics.percentage_of_students_whose_first_language_is_not_french ?? 'N/A'}%</p>
                        <p className="text-xs">New to Canada (non-FR): {demo.demographics.percentage_of_students_who_are_new_to_canada_from_a_non_french_speaking_country ?? 'N/A'}%</p>
                      </>
                    ) : (
                      <>
                        <p className="text-xs">First language not English: {demo.demographics.percentage_of_students_whose_first_language_is_not_english ?? 'N/A'}%</p>
                        <p className="text-xs">New to Canada (non-EN): {demo.demographics.percentage_of_students_who_are_new_to_canada_from_a_non_english_speaking_country ?? 'N/A'}%</p>
                      </>
                    )}
                    <p className="text-xs">Special education: {demo.demographics.percentage_of_students_receiving_special_education_services ?? 'N/A'}%</p>
                    <p className="text-xs">Gifted: {demo.demographics.percentage_of_students_identified_as_gifted ?? 'N/A'}%</p>
                    <p className="text-xs">Parents w/o degree: {demo.demographics.percentage_of_students_whose_parents_have_no_degree__diploma_or_certificate ?? 'N/A'}%</p>
                  </>
                )}
              </div>
            )}
          </div>
        </LazyPopup>
      </CircleMarker>
    );
  };

  return (
    <div className={`relative ${className || ''}`}>
      <MapContainer
        center={MAP_DEFAULT_CENTER}
        zoom={MAP_DEFAULT_ZOOM}
        maxBounds={ONTARIO_BOUNDS}
        maxBoundsViscosity={1.0}
        minZoom={MAP_MIN_ZOOM}
        maxZoom={MAP_MAX_ZOOM}
        className={isFullscreen ? 'h-full w-full' : 'h-96 md:h-[60vh] lg:h-[75vh] w-full rounded-lg'}
        zoomControl={false}
        preferCanvas={true}
      >
        <MapEventHandler onBoundsChange={handleBoundsChange} />
        <TileLayer
          attribution='&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
          url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
        />
        {/* Only render zoom control for the visible map instance */}
        {(isFullscreen || !isMapFullscreen) && <ZoomControl position="bottomright" />}

        {/* Render aggregated case markers — culled to current viewport */}
        {visibleCases.map((schoolCase) => renderCaseMarker(schoolCase))}

        {/* Render closure markers — culled to current viewport */}
        {visibleClosures.map((closure) => renderClosureMarker(closure))}
      </MapContainer>

      {/* Fullscreen toggle button — hidden when already in fullscreen overlay */}
      {!isFullscreen && <MapFullscreenButton />}

      {/* Legend — bottom-left so it never collides with the FloatingFilterPanel (top-left) in fullscreen */}
      <div className="absolute bottom-4 left-4 bg-white p-3 rounded-lg shadow-lg border z-[400]">
        <h3 className="font-semibold text-sm mb-2">Legend</h3>
        <div className="space-y-2 text-xs">
          <div className="flex items-center gap-2">
            <div className="w-4 h-4 rounded-full" style={{ backgroundColor: CASE_COLOR_LOW }}></div>
            <span>Cases (1–5)</span>
          </div>
          <div className="flex items-center gap-2">
            <div className="w-4 h-4 rounded-full" style={{ backgroundColor: CASE_COLOR_HIGH }}></div>
            <span>Cases (6+)</span>
          </div>
          <div className="flex items-center gap-2">
            <div className="w-4 h-4 rounded-full" style={{ backgroundColor: CASE_COLOR_CLOSURE }}></div>
            <span>School Closures</span>
          </div>
        </div>
      </div>
    </div>
  );
}
