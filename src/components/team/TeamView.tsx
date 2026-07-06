'use client';

import Image from 'next/image';

export default function TeamView() {
  return (
    <div className="bg-white rounded-lg shadow p-6 max-w-3xl mx-auto">
      <h2 className="text-3xl font-bold mb-4 text-black">The Team and Contact</h2>
      <section className="mb-8">
        <h3 className="text-xl font-semibold mb-2">Dr. Prachi Srivastava</h3>
        <p className="italic">Principal Investigator and Project Lead</p>
        <p className="mb-2">Dr. Prachi Srivastava is Associate Professor specialising in education policy and global development. She first conceptualised and led the development of the COVID-19 School Dashboard in 2020 as an emergency response to the pandemic in Ontario. She continues to provide intellectual leadership to the project for data integration, refinement, and analysis.</p>
        <p className="mb-2">Globally, Dr. Srivastava led high-level policy briefs on education policy and planning and equity implications for recovery for the 2020, 2021, and 2022 G20 Summit processes. She provided expertise for the UN Department of Economic and Social Affairs, UNESCO, UNICEF Office of Global Insight and Policy, BE2 education donor working group, and global CSOs and NGOs on these issues. In Canada, she was an invited expert on education policy for the Ontario COVID-19 Science Advisory Table.</p>
        <p className="mb-2">Dr. Srivastava&apos;s work and analysis on education and the pandemic have been frequently and widely covered in radio, print, and online media, including: CBC News, CBC The National, CTV News, Global News, Maclean&apos;s, The Globe and Mail, and others.</p>
        <p className="mb-2">The COVID-19 School Dashboard was one of only 11 projects in Canada to be awarded a grant from the New Digital Research Infrastructure Organization (NDRIO)-Portage (The Digital Alliance) COVID-19 Data Curation Funding Scheme. The Dashboard was featured at the 2021 United Nations World Data Forum.</p>
        <p className="mb-2">Dr. Srivastava is a faculty member at the Adelaide University and holds a doctorate from the University of Oxford.</p>
        <p className="mb-2">Contact her for all project-related inquiries.</p>
        <div className="flex items-center gap-4 mb-2">
          <a href="mailto:prachi.srivastava@adelaide.edu.au" className="text-blue-700 underline">prachi.srivastava@adelaide.edu.au</a>
        </div>
      </section>
      <hr className="my-6" />
      <section className="mb-8 grid grid-cols-1 md:grid-cols-2 gap-8">
        <div>
          <h4 className="font-semibold">Connor Cozens</h4>
          <p className="italic">Lead Software Developer</p>
          <p>Connor has actively contributed to the technical development of the COVID-19 School Dashboard since 2020. He has worked at a range of startups and in big tech. He graduated from Western University with a BSc (Honours) in Computer Science and a Minor in Software Engineering in 2021. Contact Connor for any technical queries.</p>
          <p className="mt-2">Contact: <a href="mailto:cozcon@gmail.com" className="text-blue-700 underline">cozcon@gmail.com</a></p>
          <div className="flex items-center gap-4 mt-1">
            <a href="https://github.com/connor-cozens" target="_blank" rel="noopener noreferrer">
              <Image src="/github_logo.png" alt="GitHub" width={32} height={32} className="h-8 w-8 inline" />
            </a>
          </div>
        </div>
      </section>
      <hr className="my-6" />
      <section className="mb-8">
        <h3 className="text-lg font-semibold mb-2">Past Members</h3>
        <ul className="list-disc list-inside text-gray-700">
          <li><span className="font-semibold">Kelly Bairos</span>, Data Curation and Archiving, Project Coordinator</li>
          <li><span className="font-semibold">Nathan T.T. Lau</span>, Data Curation and Archiving, Post-doctoral Fellow (Research Engineer)</li>
          <li><span className="font-semibold">Justin Marshall</span>, Developer</li>
          <li><span className="font-semibold">Peter J. Taylor</span>, Lead Developer and Technical Advisor</li>
          <li><span className="font-semibold">Anushka Khanna</span>, Project Coordination, Research Associate</li>
          <li><span className="font-semibold">Claire Wu</span>, Assistant Developer</li>
        </ul>
      </section>
      <hr className="my-6" />
    </div>
  );
} 