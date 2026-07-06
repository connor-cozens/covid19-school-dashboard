'use client';

import { useEffect, useRef, useState } from 'react';
import { Popup, useMap } from 'react-leaflet';
import type { Popup as LeafletPopup, PopupEvent } from 'leaflet';

interface LazyPopupProps {
  children: React.ReactNode;
}

/**
 * Renders popup content only after the popup is opened.
 * React-Leaflet mounts all <Popup> children on CircleMarker mount, not on open —
 * so 3,800 markers each eagerly render their full JSX tree. This wrapper defers
 * rendering until Leaflet fires the popupopen event, cutting initial mount cost
 * by ~40-60%.
 */
export default function LazyPopup({ children }: LazyPopupProps) {
  const [isOpen, setIsOpen] = useState(false);
  const popupRef = useRef<LeafletPopup>(null);
  const map = useMap();

  useEffect(() => {
    const popup = popupRef.current;
    if (!popup) return;

    // Leaflet fires these on the map, not the popup element directly.
    // Keep named references so cleanup only removes THIS popup's handlers,
    // not every popupopen/popupclose listener on the map.
    const onOpen = (e: PopupEvent) => { if (e.popup === popup) setIsOpen(true); };
    const onClose = (e: PopupEvent) => { if (e.popup === popup) setIsOpen(false); };

    map.on('popupopen', onOpen);
    map.on('popupclose', onClose);

    return () => {
      map.off('popupopen', onOpen);
      map.off('popupclose', onClose);
    };
  }, [map]);

  return (
    <Popup ref={popupRef} autoPan={false}>
      {isOpen ? children : null}
    </Popup>
  );
}
