FROM erlang:27

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    git \
    curl \
    ca-certificates \
    unixodbc-dev \
    libssl-dev \
    libexpat1-dev \
    libyaml-dev \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Copy only what we need (dockerignore will protect us)
COPY . .

# Build MongooseIM release
RUN make rel

# ---- Runtime image (smaller) ----
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    openssl \
    unixodbc \
    libssl3 \
    libexpat1 \
    libyaml-0-2 \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/mongooseim

# Copy built release
COPY --from=0 /build/_build/prod/rel/mongooseim .

EXPOSE 5222 5269 5280

CMD ["bin/mongooseim", "foreground"]
