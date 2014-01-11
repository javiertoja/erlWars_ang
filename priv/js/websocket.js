var websocket;

function conectar()
{
    
    wsHost = "ws://127.0.0.1:8080/websocket";
    websocket = new WebSocket(wsHost);
    websocket.onopen = function(evt) { onOpen(evt) }; 
    websocket.onclose = function(evt) { onClose(evt) }; 
    websocket.onmessage = function(evt) { onMessage(evt) }; 
    websocket.onerror = function(evt) { onError(evt) }; 
};  

function desconectar() {
    websocket.close();
}; 

function sendMsg(msg) {
    if(websocket.readyState == websocket.OPEN){
        
        websocket.send(msg);
    }
    else {
         error('websocket conectado no está disponible.'); 
    };
};

function onOpen(evt) { 
    conectado();      
};  

function onClose(evt) { 
  desconexion();
};  

function onMessage(evt) {
  var array = evt.data.split("&");
  var mensaje = array[0];
  var parametros = array.slice(1);

  if (mensaje == "partidas"){
    for(i = 0; i < parametros.length; i++)
      if(parametros[i])
        anadirPartidaALista(parametros[i]); 
  }

  if (mensaje == "estadoPartidaModificado"){
    borrarPartidas();
    for(i = 0; i < parametros.length; i++)
      if(parametros[i])
        anadirPartidaALista(parametros[i]); 
  }

  if (mensaje == "partidaCreada")
    initPartida();

  if (mensaje == "unidoAPartida")
    initPartida();

  if (mensaje == "turno")
    turno();

  if (mensaje == "noturno")
      finTurno();

  if (mensaje == "campeon")
    ganador();

  if (mensaje == "perdedor")
    perdedor();

  if (mensaje == "notificacionDisparado")
    recibirDisparo(parametros[0],parametros[1]-1,parametros[2]-1,parametros[3]=="true",parametros[4]=="true");
};  

function onError(evt) {
   error(evt.data);
};

function obtenerPartidas () {
  sendMsg("partidas");
}

function unirseAPartida (nick, nombrePartida) {
  sendMsg("unirseAPartida,"+nick+","+nombrePartida)
}

function wsCrearPartida (nick, nombrePartida) {
  sendMsg("crearPartida,"+nick+","+nombrePartida);
}

function wsdisparo (nick,nombrePartida,x,y) {
  sendMsg("disparo,"+nick+","+nombrePartida+","+(x+1)+","+(y+1));
}

function posicionarNave (nick,nombrePartida,x,y) {
  sendMsg("posicionarNave,"+nick+","+nombrePartida+","+(x+1)+","+(y+1));
}
