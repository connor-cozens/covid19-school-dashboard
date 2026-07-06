import React from 'react';

export default function DataSourcesView() {
  return (
    <div className="bg-white rounded-lg shadow p-6 max-w-3xl mx-auto">
      <h2 className="text-3xl font-bold mb-4 text-black">Data Sources & Code</h2>
      <section className="mb-8">
        <h3 className="text-xl font-semibold mb-2">Data Sources 2021-22</h3>
        <ul className="list-disc pl-6 space-y-2">
          <li><a href="https://data.ontario.ca/dataset?keywords_en=COVID-19" target="_blank" rel="noopener noreferrer" className="text-blue-700 underline">All COVID-19 datasets</a></li>
          <li><a href="https://www.ontario.ca/page/covid-19-cases-schools-and-child-care-centres" target="_blank" rel="noopener noreferrer" className="text-blue-700 underline">COVID-19 cases in schools and child care centres</a></li>
          <li><a href="https://data.ontario.ca/dataset/summary-of-cases-in-schools" target="_blank" rel="noopener noreferrer" className="text-blue-700 underline">Schools COVID-19 data overview</a></li>
          <li><a href="https://data.ontario.ca/dataset/d85f68c5-fcb0-4b4d-aec5-3047db47dcd5/resource/602a5186-67f5-4faf-94f3-7c61ffc4719a/download/new_sif_data_table_2018_2019prelim_en_august.xlsx" target="_blank" rel="noopener noreferrer" className="text-blue-700 underline">School information and student demographics dataset (.xlsx)</a></li>
          <li><a href="https://data.ontario.ca/dataset/school-information-and-student-demographics" target="_blank" rel="noopener noreferrer" className="text-blue-700 underline">School information and student demographics overview</a></li>
          <li><a href="https://data.ontario.ca/dataset/b1fef838-8784-4338-8ef9-ae7cfd405b41/resource/8b6d22e2-7065-4b0f-966f-02640be366f2/download/schoolsactivecovid.csv" target="_blank" rel="noopener noreferrer" className="text-blue-700 underline">Schools with active COVID-19 cases dataset (.csv)</a></li>
          <li><a href="https://data.ontario.ca/dataset/b1fef838-8784-4338-8ef9-ae7cfd405b41/resource/7fbdbb48-d074-45d9-93cb-f7de58950418/download/schoolcovidsummary.csv" target="_blank" rel="noopener noreferrer" className="text-blue-700 underline">Summary of cases in schools dataset (.csv)</a></li>
        </ul>
        <br />
        <p>Special dataset on individual school closures in Ontario reported by the Ministry of Education Ontario (not available for public release).</p>
      </section>
      <section className="mb-8">
        <h3 className="text-xl font-semibold mb-2">Source Code</h3>
        <p>Source code for this site can be found <a href="https://github.com/connor-cozens/covid19-school-dashboard" target="_blank" rel="noopener noreferrer" className="text-blue-700 underline">here</a>.</p>
      </section>
      <section className="mb-8">
        <h3 className="text-xl font-semibold mb-2">Archive</h3>
        <p>The archival material, code for the site can be found <a href="https://doi.org/10.5683/SP3/Z9SNP0" target="_blank" rel="noopener noreferrer" className="text-blue-700 underline">here</a> and the integrated dataset can be found <a href="https://doi.org/10.5683/SP3/D0QXGQ" target="_blank" rel="noopener noreferrer" className="text-blue-700 underline">here</a>.</p>
      </section>
    </div>
  );
} 