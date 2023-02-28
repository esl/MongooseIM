
current_ws_addr = "ws://" + window.location.host + "/ws-traffic";
connected = false
const SOCKET_STATES = {connecting: 0, open: 1, closing: 2, closed: 3}

start_app()
initialise()

function open_websocket() {
    socket = new WebSocket(current_ws_addr);
    return socket
}

function push(event, payload) {
    m = JSON.stringify({ event : event, payload : payload})
    if(event != "heartbeat") console.log(`--->: ${m}`);
    socket.send(m)
}

function sendHeartbeat(socket) {
  if(!isConnected(socket)){
      initialise()
      return }
    if(socket.pendingHeartbeatRef){
      socket.pendingHeartbeatRef = null
      console.log("heartbeat timeout. Attempting to re-establish connection")
      clearInterval(socket.heartbeatTimer)
      abnormalClose(socket, "heartbeat timeout")
      initialise()
      return
    }
    socket.pendingHeartbeatRef = true
    push("heartbeat", {})
  }

function isConnected(socket){ return connectionState(socket) === "open" }

function abnormalClose(socket, reason){
    socket.closeWasClean = false
    socket.close(1000, reason)
}


function  connectionState(socket){
    switch(socket.readyState){
      case SOCKET_STATES.connecting: return "connecting"
      case SOCKET_STATES.open:       return "open"
      case SOCKET_STATES.closing:    return "closing"
      default:                       return "closed"
    }
  }


function initialise() {
    socket = open_websocket()
    console.log(socket)
    socket.onopen = function(e) {
      console.log("[open] Connection established");
      socket.onclose = event => {
          app.ports.incPort.send({"event":"connection_lost", "payload":{}})
      };
      socket.pendingHeartbeatRef = null
      clearInterval(socket.heartbeatTimer)
      socket.heartbeatTimer = setInterval(() => sendHeartbeat(socket), 2000)
      if(connected) {
          app.ports.incPort.send({"event":"reinitialise", "payload":{}})
      }else{
          app.ports.incPort.send({"event":"initialise", "payload":{}})
          connected = true
      }

    };
    socket.onmessage = function(event) {
        socket.pendingHeartbeatRef = null
        e = JSON.parse(event.data)
        if(e.event != "heartbeat_ok"){
            console.log(`    <---: ${event.data}`);
            handle_event(e)
        }
    };
    if(!connected) {
        socket.onclose = event => {
            app.ports.incPort.send({"event":"connection_failed", "payload":{}})
        };
    };
}


function start_app() {
    app = Elm.Traffic.init({ node: document.getElementById("session-elm-container"),
                             flags: current_ws_addr})
    app.ports.outPort.subscribe(function(data){
        if(data[0] == "change_connection") {
            change_connection(data[1].addr)
        }else {
        push(data[0], data[1]);
        }
    })
}

function handle_event(e){
  app.ports.incPort.send(e)
}

function change_connection(addr) {
    if(addr != current_ws_addr){
        current_ws_addr = addr;
        connected = false;
        initialise();
    }
}
