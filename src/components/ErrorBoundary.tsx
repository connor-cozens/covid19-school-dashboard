'use client';

import { Component, ReactNode } from 'react';

interface Props {
  children: ReactNode;
  fallback?: ReactNode;
}

interface State {
  hasError: boolean;
  message: string;
}

/**
 * Catches unhandled render errors anywhere in the component tree and shows
 * a fallback UI instead of a blank page.  Wrap around high-risk subtrees
 * (map, charts) so an error in one section doesn't take down the whole app.
 */
export default class ErrorBoundary extends Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = { hasError: false, message: '' };
  }

  static getDerivedStateFromError(error: Error): State {
    return { hasError: true, message: error.message };
  }

  componentDidCatch(error: Error, info: { componentStack: string }) {
    console.error('[ErrorBoundary] Caught error:', error, info.componentStack);
  }

  render() {
    if (this.state.hasError) {
      return this.props.fallback ?? (
        <div className="bg-red-50 border border-red-200 rounded-lg p-6 my-4">
          <h3 className="text-lg font-medium text-red-800">Something went wrong</h3>
          <p className="text-red-600 mt-2 text-sm">{this.state.message}</p>
          <button
            className="mt-4 text-sm text-blue-600 underline"
            onClick={() => window.location.reload()}
          >
            Reload the page
          </button>
        </div>
      );
    }
    return this.props.children;
  }
}
