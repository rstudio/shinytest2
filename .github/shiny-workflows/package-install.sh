# pak installs `chromium` from the `ppa:xtradeb/apps` Launchpad PPA, which is
# often down. GitHub's Ubuntu runners already ship Google Chrome, so skip pak's
# system requirements and install the rest of them ourselves.
# ponytail: hardcoded list from pak's sysreqs output; update if a new
# dependency needs a system library.
if [ "$RUNNER_OS" = "Linux" ]; then
  echo "PKG_SYSREQS=false" >> "$GITHUB_ENV"
  sudo apt-get update
  sudo apt-get install -y \
    libx11-dev git libcurl4-openssl-dev libssl-dev cmake make libuv1-dev \
    libgit2-dev zlib1g-dev pandoc libpng-dev libfreetype6-dev libjpeg-dev \
    libtiff-dev libwebp-dev libicu-dev libfontconfig1-dev libfribidi-dev \
    libharfbuzz-dev libxml2-dev
fi
