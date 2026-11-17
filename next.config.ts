import type { NextConfig } from "next";

const nextConfig: NextConfig = {
  // Fully static site: `next build` writes plain HTML/JS/data to out/,
  // which is deployed to GitHub Pages. There is no server at runtime.
  output: 'export',
  images: {
    // Image optimization needs a server; the site only uses a few small logos
    unoptimized: true,
  },
};

export default nextConfig;
