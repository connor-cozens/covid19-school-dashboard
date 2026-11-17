import type { Metadata } from 'next';
import MediaView from '@/components/media/MediaView';

export const metadata: Metadata = { title: 'Media' };

export default function Page() {
  return <MediaView />;
}
