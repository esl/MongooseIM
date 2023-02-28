
function open_websocket() {
    socket = new WebSocket("ws://localhost:{{{ traffic_channel_port}}}/ws-traffic")
    return socket
}
