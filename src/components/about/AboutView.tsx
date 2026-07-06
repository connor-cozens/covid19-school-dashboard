'use client';

import Image from 'next/image';

export default function AboutView() {
  return (
    <div className="bg-white rounded-lg shadow p-6 max-w-3xl mx-auto">
      <h2 className="text-3xl font-bold mb-4 text-black">About This Site</h2>
      <section className="mb-8">
        <h3 className="text-xl font-semibold mb-2">COVID-19 School Dashboard: Key Aims & Information</h3>
        <p className="mb-2">
          <a href="http://covid19schooldashboard.com" target="_blank" rel="noopener noreferrer" className="text-blue-700 underline">covid19schooldashboard.com</a> reports and maps confirmed school-related cases of COVID-19 in publicly funded elementary and secondary schools in Ontario, Canada, and connects this to data on school social background characteristics. The site covers case data from September 2020 to April 2021 (2020–21 school year) and August 2021 to December 2021 (2021–22 school year), the last dates for which school infection data for Ontario are publicly available. School closure data covers an additional period through May 2022.
        </p>
        <p className="mb-2">First launched in 2020 for broad public dissemination, the original aim of the Dashboard was to increase transparency and understanding of the education scenario as it evolved. It provided real-time data visualization of schools with reported cases based on official publicly released data. The COVID-19 School Dashboard has been accessed thousands of times by school communities (e.g., parents, students, teachers and staff, leaders and administrators), community members, education and health professionals, officials, researchers, media, and the general public.</p>
        <p className="mb-2">The site is best viewed on a desktop or tablet.</p>
        <h4 className="font-semibold mt-4 mb-1">Why is this important?</h4>
        <p className="mb-2">Research shows the effects of COVID-19 to have been more severe on high-risk communities, populations, and schools. There are strong equity concerns. Visualizing COVID-19 case and school closure data with data on school social background characteristics helps to give a better understanding of the composition of affected schools. In short, we can get to a more fine-grained understanding of the human dimension of COVID-19 on school populations.</p>
      </section>
      <section className="mb-8">
        <h4 className="font-semibold mb-1">Update Frequency</h4>
        <p className="mb-2">This site is no longer automatically updated as the <a href="https://data.ontario.ca/dataset/summary-of-cases-in-schools" target="_blank" rel="noopener noreferrer" className="text-blue-700 underline">Ministry of Education ceased reporting data on school infections as of January 2022</a>. From September 2020 to December 2021, this site was automatically updated every weekday (excluding public holidays) following the release of school-related COVID-19 case data by the Ontario Ministry of Education. This site also used the latest publicly available data on school information and student demographics released by the Ontario Ministry of Education for school background characteristics.</p>
        <h4 className="font-semibold mt-4 mb-1">Archive of the COVID-19 School Dashboard</h4>
        <p className="mb-2">The archival material, code for the site can be found <a href="https://doi.org/10.5683/SP3/Z9SNP0" target="_blank" rel="noopener noreferrer" className="text-blue-700 underline">here</a>, and the integrated dataset can be found <a href="https://doi.org/10.5683/SP3/D0QXGQ" target="_blank" rel="noopener noreferrer" className="text-blue-700 underline">here</a>. See also the Data Sources tab for more information on data sources used.</p>
      </section>
      <section className="mb-8">
        <h3 className="text-xl font-semibold mb-2">Policy Context</h3>
        <p className="mb-2">Pandemic-related school closures in Ontario affected over 2 million elementary and secondary school students. The situation for students and schools evolved rapidly. The following provides a brief policy context of provincial policy responses on school closures and reopening. It does not outline decisions of individual school boards or regional public health units (PHUs), unless they were named in provincial announcements.</p>
        <div className="flex flex-col items-center my-4">
          <Image
            src="/timeline.png"
            alt="Ontario School Closures Timeline"
            width={800}
            height={400}
            className="rounded-lg shadow max-w-full w-[80%]"
          />
          <p className="text-xs text-gray-600 mt-2">Figure: Ontario-level school closures and reopening policy tracing (March 2020 – April 2021). Source: Official provincial government announcements and ICES COVID-19 Dashboard.</p>
        </div>
      </section>
      <section className="mb-8">
        <h4 className="font-semibold mb-1">School Year Summaries</h4>
        <ul className="list-disc pl-6 mb-2">
          <li><span className="font-bold">2021-22:</span> Cumulative totals represent all total cases reported as of 23 December 2021. The data were available for public access and download as of 14 September 2021. The first reported date of school-level cases was 26 August 2021. The last reported date was 23 December 2021. No public data were made available by the Ministry as of January 2022.</li>
          <li><span className="font-bold">2020-21:</span> Cumulative totals represent all total cases reported to the Ministry of Education as of 5 September 2020, including resolved cases. The first school-related cases appeared in the dataset on 10 September 2020. The last official data release was 27 April 2021.</li>
          <li><span className="font-bold">2019-20:</span> The first school closure announcement in Ontario was issued on 12 March 2020 for an initial period from 14 March to 4 April 2020. Public school closures were extended three times, finally until the end of June 2020.</li>
        </ul>
      </section>
      <section className="mb-8">
        <h4 className="font-semibold mb-1">Caveats</h4>
        <ul className="list-disc pl-6 mb-2">
          <li>The dashboard shows which schools are affected by confirmed cases as reported in the official data, visually plots where the schools are, and shows relevant school background characteristics. This site should not be used to draw inferences on the broader COVID-19 situation in Ontario, or on case numbers generally.</li>
          <li>Numbers of cases are extracted from official data sources. Contextual factors (e.g., testing frequency, vaccination rates, school closures/reopenings) affect data changes and may cause spikes or dips.</li>
          <li>There were known lags in data reported in the Ministry of Education dataset, which may have resulted in discrepancies in the reported data.</li>
          <li>School demographic data is drawn from official Ministry of Education records and may contain inaccuracies present in the source data.</li>
        </ul>
      </section>
      <section className="mb-8">
        <h4 className="font-semibold mb-1">School Closures Data</h4>
        <p className="mb-2">
          The dashboard maps school closures reported by the Ontario Ministry of Education across three periods: September 2020 – April 2021, September – December 2021, and January – May 2022. In total, 621 individual school closure events are included. Closures are categorised by reason, including closures directed by a Public Health Unit (PHU), decisions made by the school board, joint PHU and school board decisions, and operational closures.
        </p>
        <p className="mb-2">
          Where a reopening date was missing from the source data, a <strong>14-day estimated reopening date</strong> (closure date + 14 days) is used as a fallback. This matches the approach used in the original R Shiny version of this dashboard. Note that for the January–May 2022 closure period, the source data did not include any reopening dates, so <em>all</em> closures in that period use the 14-day estimate. These estimated dates are used only to control when a closure marker appears and disappears on the map; they do not reflect confirmed school reopening information.
        </p>
        <p className="mb-2">
          School closure markers are displayed on the same interactive map as case data. Use the <strong>Show School Closures</strong> toggle in the filter panel to show or hide them. When viewing a date range, a closure appears on the map if it was active at any point during that range. When using the day-by-day animation, a closure appears only on days it was actively open (i.e., on or after its closure date and on or before its reopening date).
        </p>
      </section>
      <section className="mb-8">
        <h4 className="font-semibold mb-1">How to Use This Dashboard</h4>
        <ul className="list-disc pl-6 mb-2">
          <li>Use the map to view affected schools. <strong>Click</strong> on a school bubble to see school-specific case counts and demographic data.</li>
          <li>Customize which schools you see using the filter panel: toggle cases and school closures on or off, filter by school board or municipality, and show or hide demographic data in popups.</li>
          <li>Use the <strong>Start Date</strong> and <strong>End Date</strong> inputs to view a specific date range. Press the play button to animate cases and closures day by day from your selected start date.</li>
          <li>Use the <strong>Summary &amp; Analytics</strong> tab for key statistics, trend charts, and a searchable data table of all school records.</li>
        </ul>
      </section>
      <section className="mb-8">
        <h4 className="font-semibold mb-1">Authorship, Attribution, Citation</h4>
        <p className="mb-2">Cite the COVID-19 School Dashboard as:<br />
          Srivastava, P., Cozens, C., Wu, C., Marshall, J., & Taylor, P.J. (2026). <em>COVID-19 school dashboard (2.1 March 2026).</em> [Web application]. <a href="http://covid19schooldashboard.com/" target="_blank" rel="noopener noreferrer" className="text-blue-700 underline">http://covid19schooldashboard.com/</a>
        </p>
        <p className="mb-2">Dr. Prachi Srivastava, Adelaide University, Australia.<br />
          <a href="mailto:prachi.srivastava@adelaide.edu.au" className="text-blue-700 underline">prachi.srivastava@adelaide.edu.au</a> | <a href="https://orcid.org/0000-0003-4865-8963" target="_blank" rel="noopener noreferrer" className="text-blue-700 underline">ORCID iD: 0000-0003-4865-8963</a>
        </p>
        <p className="mb-2 font-bold">Development:</p>
        <p className="mb-2">Connor Cozens | (<a href="mailto:cozcon@gmail.com" className="text-blue-700 underline">cozcon@gmail.com</a>)</p>
        <p className="mb-2">Justin Marshall | (<a href="mailto:powtatow@gmail.com" className="text-blue-700 underline">powtatow@gmail.com</a>)</p>
        <br />
        <p className="mb-2">Assistant Developer: Claire Wu</p>
        <br />
        <p className="mb-2">Original Technical Lead: Peter J. Taylor</p>
      </section>
    </div>
  );
} 