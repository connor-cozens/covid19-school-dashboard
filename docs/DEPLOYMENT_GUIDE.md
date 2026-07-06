# Deployment Guide — COVID-19 Ontario School Dashboard

The application deploys to **Google Cloud Run** — a fully managed container platform that scales to zero when idle and handles SSL automatically. No server management required.

---

## Prerequisites (one-time)

### 1. Install gcloud CLI

```bash
# macOS (Homebrew)
brew install --cask google-cloud-sdk

# Linux
curl https://sdk.cloud.google.com | bash
exec -l $SHELL

# Windows — download installer:
# https://cloud.google.com/sdk/docs/install#windows
```

### 2. Authenticate

```bash
gcloud auth login
gcloud auth configure-docker   # allows Docker to push to gcr.io
```

### 3. Install Docker

Download Docker Desktop: https://docs.docker.com/get-docker/

---

## Deploy

### First deploy (new GCP project)

**1. Set up your config file** (one-time):

```bash
# Production
cp .env.example .env.production
# Edit .env.production and set PROJECT_ID=your-gcp-project-id

# Dev/staging (optional — separate GCP project)
cp .env.example .env.development
# Edit .env.development and set PROJECT_ID=your-staging-project-id
```

These are the same files Next.js uses for runtime config. Deployment vars (`PROJECT_ID`, `REGION`) live alongside any future `NEXT_PUBLIC_*` vars.

**2. Deploy**:

```bash
npm run deploy         # reads .env.production
npm run deploy -- --dev  # reads .env.development
```

The script will:
1. Verify prerequisites (gcloud auth, Docker daemon)
2. Enable required GCP APIs
3. Build a Docker image tagged with the current git SHA
4. Push to Google Container Registry (`gcr.io`)
5. Deploy to Cloud Run in `us-central1` (configurable)
6. Print the live URL

### Redeploy (after code changes)

```bash
PROJECT_ID=your-gcp-project-id npm run deploy
# or directly
PROJECT_ID=your-gcp-project-id ./scripts/setup/deploy.sh
```

Each deploy creates a new revision tagged with the git commit SHA. Cloud Run keeps previous revisions available for instant rollback.

### Deploy options

```bash
# Different region
./scripts/setup/deploy.sh --project my-project --region northamerica-northeast1

# Using environment variables
export PROJECT_ID=my-project
export REGION=northamerica-northeast1
npm run deploy
```

---

## Rollback

```bash
# List revisions
gcloud run revisions list --service covid-dashboard --region us-central1

# Roll back to a specific revision
gcloud run services update-traffic covid-dashboard \
  --region us-central1 \
  --to-revisions REVISION_NAME=100
```

---

## Custom Domain (optional)

```bash
gcloud run domain-mappings create \
  --service covid-dashboard \
  --domain your-domain.com \
  --region us-central1
```

Cloud Run provisions an SSL certificate automatically. Add the DNS records it shows you to your domain registrar.

---

## CI/CD with Cloud Build (optional)

`cloudbuild.yaml` is included for automated deployments on push. To activate:

```bash
# One-time setup — create a trigger on pushes to React-Overhaul
gcloud builds triggers create github \
  --repo-name=c19-beta-site \
  --repo-owner=connor-cozens \
  --branch-pattern="^React-Overhaul$" \
  --build-config=cloudbuild.yaml \
  --project=your-gcp-project-id
```

After this, every merge to `React-Overhaul` automatically builds and deploys. No manual script needed.

**Note:** Grant the Cloud Build service account the Cloud Run Admin and Service Account User roles so it can deploy:
```bash
PROJECT_NUMBER=$(gcloud projects describe $PROJECT_ID --format="value(projectNumber)")
CB_SA="$PROJECT_NUMBER@cloudbuild.gserviceaccount.com"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:$CB_SA" \
  --role="roles/run.admin"

gcloud iam service-accounts add-iam-policy-binding \
  $PROJECT_NUMBER-compute@developer.gserviceaccount.com \
  --member="serviceAccount:$CB_SA" \
  --role="roles/iam.serviceAccountUser"
```

---

## Monitoring

```bash
# Stream live logs
gcloud run services logs tail covid-dashboard --region us-central1

# Query recent errors
gcloud logging read \
  "resource.type=cloud_run_revision AND resource.labels.service_name=covid-dashboard AND severity>=ERROR" \
  --limit 50

# Get service status and URL
gcloud run services describe covid-dashboard --region us-central1
```

---

## Troubleshooting

| Problem | Likely cause | Fix |
|---|---|---|
| `permission denied` pushing to gcr.io | Docker not auth'd | `gcloud auth configure-docker` |
| `API not enabled` error | First deploy, APIs not yet enabled | Script enables them automatically; wait 30s and retry |
| Build fails inside Docker | `npm run build` error (regression check, TypeScript, etc.) | Run `npm run build` locally to see the error |
| Service URL returns 503 | Container failing to start | Check logs: `gcloud run services logs tail covid-dashboard --region us-central1` |
| Slow cold start | Min instances set to 0 | Set `--min-instances 1` in `scripts/setup/deploy.sh` if cold starts are a problem |

### Test the build locally before deploying

```bash
# Run the full build (includes prebuild checks)
npm install && npm run build

# Test the Docker image locally
docker build --platform linux/amd64 -t covid-dashboard-test .
docker run --rm -p 3000:3000 covid-dashboard-test
# Open http://localhost:3000
```

---

## Resource configuration

Current Cloud Run settings (adjust in `scripts/setup/deploy.sh`):

| Setting | Value | Notes |
|---|---|---|
| Memory | 1 Gi | Sufficient for Next.js + data files |
| CPU | 1 vCPU | Adequate for typical traffic |
| Min instances | 0 | Scales to zero — cold start ~2-3s |
| Max instances | 10 | Caps runaway costs |
| Concurrency | 80 | Requests per instance |

Estimated cost (Cloud Run):
- Idle: ~$0/month (scales to zero)
- Light traffic: ~$5–20/month
- Moderate traffic: ~$20–100/month
