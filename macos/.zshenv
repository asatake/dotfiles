source ~/.profile
source ~/.zprofile
# . "$HOME/.cargo/env"

export LANG="ja_JP.UTF-8"
export STARSHIP_CACHE=~/.starship/cache
export STARSHIP_CONFIG=~/.config/starship.toml

export WINIT_UNIX_BACKEND=x11
export LSP_USE_PLISTS=true
export GOPRIVATE="github.com/pokerroom/*"
export PATH="/usr/local/opt/mysql-client/bin:$PATH"
# Added by serverless binary installer
export PATH="$HOME/.serverless/bin:$PATH"
export PATH="/opt/homebrew/opt/curl/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="/opt/homebrew/opt/mysql@8.4/bin:$PATH"

export LIBRARY_PATH="/usr/lib"
export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix libgccjit)/lib/gcc/15"
export LIBRARY_PATH="$LIBRARY_PATH:/usr/local/lib"
export CPATH="$(brew --prefix libgccjit)/include"

# LaTeX
export PATH="/Library/Tex/texbin:$PATH"

# Android
export ANDROID_HOME="$HOME/Library/Android/sdk"

export PATH="/opt/homebrew/opt/curl/bin:$PATH"

# uv
export PATH="/Users/tetsuhiromanome/.local/bin:$PATH"

# claude
export CLAUDE_CODE_ENABLE_TELEMETRY=1
export OTEL_EXPORTER_OTLP_METRICS_PROTOCOL=grpc
