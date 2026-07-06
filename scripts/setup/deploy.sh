#!/usr/bin/env bash
# scripts/setup/deploy.sh — Deploy COVID-19 School Dashboard to Google Cloud Run
#
# Usage:
#   npm run deploy                                 # production (loads .env.production)
#   npm run deploy -- --dev                        # dev/staging (loads .env.development)
#   ./scripts/setup/deploy.sh --project my-id     # one-off project override
#   ./scripts/setup/deploy.sh --help
#
# Configuration:
#   Copy .env.example to .env.production and set PROJECT_ID.
#   Copy .env.example to .env.development for a dev/staging GCP project.
#   These are the same files Next.js uses — one config for both build and deploy.
#   CLI flags and shell environment variables always override the file.
#
# Prerequisites (one-time):
#   1. Install gcloud CLI: https://cloud.google.com/sdk/docs/install
#   2. Authenticate: gcloud auth login
#   3. Install Docker: https://docs.docker.com/get-docker/

set -euo pipefail

# ── Resolve script directory (works regardless of where you call it from) ──────
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# ── Defaults (overridden by .env, then by CLI flags) ──────────────────────────
PROJECT_ID="${PROJECT_ID:-}"
REGION="${REGION:-us-central1}"
SERVICE_NAME="${SERVICE_NAME:-covid-dashboard}"
IMAGE_HOST="gcr.io"
ENV_FILE=".env.production"
# ──────────────────────────────────────────────────────────────────────────────

RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; BLUE='\033[0;34m'; NC='\033[0m'
info()    { echo -e "${GREEN}  ✓${NC}  $*"; }
warn()    { echo -e "${YELLOW}  !${NC}  $*"; }
error()   { echo -e "${RED}  ✗${NC}  $*" >&2; }
heading() { echo -e "\n${BLUE}▸ $*${NC}"; }
die()     { error "$*"; exit 1; }

show_usage() {
  cat <<EOF

Usage: $(basename "$0") [OPTIONS]

Options:
  -p, --project PROJECT_ID   GCP project ID (overrides .env)
  -r, --region  REGION       Cloud Run region (overrides .env, default: us-central1)
      --dev                  Load .env.dev instead of .env (dev/staging target)
  -h, --help                 Show this help

Config file (recommended):
  Copy .env.example → .env.production (production) or .env.development (dev/staging).
  The deploy script reads the same files Next.js uses — one config for both.

Examples:
  npm run deploy                                                  # production
  npm run deploy -- --dev                                         # dev/staging
  ./scripts/setup/deploy.sh --project my-project                 # one-off override
  ./scripts/setup/deploy.sh --project my-project --region northamerica-northeast1

EOF
}

# ── Parse args (first pass — pick up --dev before loading env file) ───────────
for arg in "$@"; do
  [[ "$arg" == "--dev" ]] && ENV_FILE=".env.development"
done

# ── Load env file (if present) — shell env vars already set take precedence ───
# Resolve relative to repo root (two levels up from scripts/setup/)
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
ENV_PATH="$REPO_ROOT/$ENV_FILE"
if [[ -f "$ENV_PATH" ]]; then
  # set -a exports every variable defined while active; source reads the file
  # in the current shell. Bash ignores comment lines (#) and blank lines.
  set -a
  # shellcheck source=/dev/null
  source "$ENV_PATH"
  set +a
  info "Loaded config from $ENV_FILE"
else
  warn "No $ENV_FILE found — copy .env.example to $ENV_FILE and set PROJECT_ID"
fi

# ── Parse args (second pass — CLI flags override env file) ────────────────────
while [[ $# -gt 0 ]]; do
  case $1 in
    -p|--project) PROJECT_ID="$2"; shift 2 ;;
    -r|--region)  REGION="$2";     shift 2 ;;
    --dev)        shift ;;   # already handled above
    -h|--help)    show_usage; exit 0 ;;
    *) error "Unknown option: $1"; show_usage; exit 1 ;;
  esac
done

# ── Validate ───────────────────────────────────────────────────────────────────
if [[ -z "$PROJECT_ID" ]]; then
  error "PROJECT_ID is not set."
  echo ""
  echo "  Option 1 — set it in $ENV_FILE (at the repo root):"
  echo "             cp .env.example $ENV_FILE"
  echo "             # then edit $ENV_FILE and set PROJECT_ID"
  echo ""
  echo "  Option 2 — pass it directly:"
  echo "             ./scripts/setup/deploy.sh --project my-project-id"
  echo ""
  exit 1
fi

IMAGE="$IMAGE_HOST/$PROJECT_ID/$SERVICE_NAME"
GIT_SHA=$(git rev-parse --short HEAD 2>/dev/null || echo "manual")

# ── Step 1: Prerequisites ──────────────────────────────────────────────────────
heading "Checking prerequisites"

for cmd in gcloud docker git; do
  if ! command -v "$cmd" &>/dev/null; then
    case "$cmd" in
      gcloud) die "gcloud CLI not found. Install: https://cloud.google.com/sdk/docs/install" ;;
      docker) die "Docker not found. Install: https://docs.docker.com/get-docker/" ;;
      git)    die "git not found." ;;
    esac
  fi
done

if ! docker info &>/dev/null; then
  die "Docker daemon is not running. Start Docker Desktop and try again."
fi

if ! gcloud auth print-access-token &>/dev/null; then
  die "Not authenticated with gcloud. Run: gcloud auth login"
fi

info "All prerequisites met"

# ── Step 2: GCP project ────────────────────────────────────────────────────────
heading "Configuring GCP project: $PROJECT_ID"

gcloud config set project "$PROJECT_ID" --quiet

info "Enabling required APIs (first run may take ~30 seconds)..."
gcloud services enable \
  run.googleapis.com \
  containerregistry.googleapis.com \
  cloudbuild.googleapis.com \
  --quiet

info "Configuring Docker authentication..."
gcloud auth configure-docker --quiet

info "GCP project ready"

# ── Step 3: Build Docker image ─────────────────────────────────────────────────
heading "Building Docker image"
echo "    Image : $IMAGE"
echo "    Tag   : $GIT_SHA"
echo "    Commit: $(git log -1 --format='%s' 2>/dev/null || echo 'unknown')"
echo ""

docker build \
  --platform linux/amd64 \
  --tag "$IMAGE:$GIT_SHA" \
  --tag "$IMAGE:latest" \
  .

info "Docker image built"

# ── Step 4: Push to Container Registry ────────────────────────────────────────
heading "Pushing image to Container Registry"

docker push "$IMAGE:$GIT_SHA"
docker push "$IMAGE:latest"

info "Image pushed: $IMAGE:$GIT_SHA"

# ── Step 5: Deploy to Cloud Run ───────────────────────────────────────────────
heading "Deploying to Cloud Run"
echo "    Service: $SERVICE_NAME"
echo "    Region : $REGION"
echo ""

gcloud run deploy "$SERVICE_NAME" \
  --image "$IMAGE:$GIT_SHA" \
  --platform managed \
  --region "$REGION" \
  --allow-unauthenticated \
  --memory 1Gi \
  --cpu 1 \
  --min-instances 0 \
  --max-instances 10 \
  --concurrency 80 \
  --port 3000 \
  --set-env-vars NODE_ENV=production \
  --quiet

# ── Done ───────────────────────────────────────────────────────────────────────
URL=$(gcloud run services describe "$SERVICE_NAME" \
  --region="$REGION" \
  --format="value(status.url)" 2>/dev/null || echo "(URL not available)")

echo ""
echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${GREEN}  Deployment complete!${NC}"
echo ""
echo "  URL:     $URL"
echo "  Service: $SERVICE_NAME"
echo "  Region:  $REGION"
echo "  Project: $PROJECT_ID"
echo "  Image:   $IMAGE:$GIT_SHA"
echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
