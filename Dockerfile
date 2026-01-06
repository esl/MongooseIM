# -------- Build stage --------
FROM erlang:27.3.4 AS builder

WORKDIR /build

# Install system deps
RUN apt-get update && apt-get install -y \
    git \
    make \
    gcc \
    libssl-dev \
    ca-certificates \
    curl \
 && rm -rf /var/lib/apt/lists/*

# Copy source
COPY . .

# Build MongooseIM release
RUN make rel

# -------- Runtime stage --------
FROM debian:12-slim

WORKDIR /opt/mongooseim

# Runtime deps
RUN apt-get update && apt-get install -y \
    libssl3 \
    openssl \
    curl \
    ca-certificates \
 && rm -rf /var/lib/apt/lists/*

# Copy built release
COPY --from=builder /build/_build/prod/rel/mongooseim ./

# Expose ports
EXPOSE 5222 5269 5280

# Healthcheck
HEALTHCHECK --interval=30s --timeout=5s --start-period=30s \
  CMD bin/mongooseim ping || exit 1

# Start server
CMD ["bin/mongooseim", "foreground"]
