# pak reads system requirement rules from r-hub/r-system-requirements, a fork
# that is synced by hand. Read them from upstream so pak gets
# rstudio/r-system-requirements#270, which skips the Launchpad Chromium install
# when Chrome is already present. Remove once the fork includes #270.
echo "R_PKG_SYSREQS_GIT_REPO_URL=https://github.com/rstudio/r-system-requirements.git" >> "$GITHUB_ENV"
