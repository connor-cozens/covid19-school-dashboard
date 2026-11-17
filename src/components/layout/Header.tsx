'use client';

import Link from 'next/link';
import { usePathname } from 'next/navigation';
import { MapIcon, ChartBarIcon, InformationCircleIcon, UserGroupIcon, NewspaperIcon } from '@heroicons/react/24/outline';
import clsx from 'clsx';

const navigation = [
  { name: 'Interactive Map', href: '/', icon: MapIcon },
  { name: 'Summary & Analytics', href: '/summary/', icon: ChartBarIcon },
  { name: 'Data Sources', href: '/data-sources/', icon: InformationCircleIcon },
  { name: 'About', href: '/about/', icon: InformationCircleIcon },
  { name: 'Media', href: '/media/', icon: NewspaperIcon },
  { name: 'Team', href: '/team/', icon: UserGroupIcon },
];

/** Normalises '/about' and '/about/' so either matches the nav entry. */
const withSlash = (path: string) => (path.endsWith('/') ? path : `${path}/`);

export default function Header() {
  const pathname = withSlash(usePathname() ?? '/');

  return (
    <header className="bg-white shadow-sm border-b border-gray-200">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex justify-between items-center h-16">
          <div className="flex items-center">
            <Link href="/" className="text-xl font-semibold text-gray-900">
              COVID-19 School Dashboard
            </Link>
          </div>

          <nav className="flex space-x-8">
            {navigation.map((item) => (
              <Link
                key={item.name}
                href={item.href}
                aria-current={pathname === item.href ? 'page' : undefined}
                className={clsx(
                  'flex items-center px-3 py-2 text-sm font-medium rounded-md transition-colors',
                  pathname === item.href
                    ? 'bg-blue-100 text-blue-700'
                    : 'text-gray-500 hover:text-gray-700 hover:bg-gray-50'
                )}
              >
                <item.icon className="h-5 w-5 mr-2" />
                {item.name}
              </Link>
            ))}
          </nav>
        </div>
      </div>
    </header>
  );
}
