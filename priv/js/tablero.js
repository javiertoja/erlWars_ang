$(document).ready(init);

var numeroNaves = 3;
var tamanoTablero = 10;
var celdaExplosion1;
var celdaExplosion2;

var nick;
var nombrePartida;

var finpartida = false;

function init(){

	// Boton On/Off Fondo
	$("#fondoOnOff").click(function(){$("#fondo").toggle();});

	// Si el navegador no soporta WebSockets
    if(!("WebSocket" in window)){  
    	$('#error').text("ATENCION: No podemos establecer comincación con el servidor.");
    }
    else {
    	// conectamos con el websocket
    	conectar();
	}
}

function conectado() {
	//mostramos el Login
	initLogin();
}

function initLogin() {
	$("#tablaPartidas").empty();
	obtenerPartidas();
	$("#textBoxPartida").keypress(validarNombre);
	$("#textBoxNick").keypress(validarNombre);
	$("#botonCrearPartida").click(crearPartida);
	$("#login").show();
}

function joinPartida (evento) {
	if (!$("#textBoxNick").val()){
		$('#error').text("Informe de su Nick para poder Crar o unirse a una partida.");
		$("#textBoxNick").focus();
	}
	else{
		$("#login").hide();

		nick = $("#textBoxNick").val();
		nombrePartida = $(this).parent().parent().find(".partida").text();
		unirseAPartida(nick,nombrePartida);
	}
}

function crearPartida (event){
	if (!$("#textBoxNick").val()){
		$('#error').text("Informe de su Nick para poder Crar o unirse a una partida.");
		$("#textBoxNick").focus();
	}
	else if (!$("#textBoxPartida").val()){
		$('#error').text("Rellene el nombre de partida para poder crearla.");
		$("#textBoxPartida").focus();
	}
	else {
		// guardamos en variables globales el login y la partida
		nick = $("#textBoxNick").val();
		nombrePartida = $("#textBoxPartida").val();
		wsCrearPartida(nick,nombrePartida);
	}
}
	

function initPartida () {
	finpartida = false;
	$('#error').html('Partida INICIADA.');
	$('#batalla').html('<h2>Batalla: '+nombrePartida+'</h2><br/>');
	$('#nick').html('<h2>Piloto: '+nick+'</h2><br/>');

	//Creamos los tableros y las naves	
	crearTablero($("#panelJuego"),tamanoTablero, false);
	crearTablero($("#panelNuestro"),tamanoTablero, true);
	anadirNaves($("#naves"),numeroNaves);

	$('#botonListo').click(listo);
	$('#botonSalir').click(salirPartida);

	$("#panelJuego .fondoModal").show();
	$("#panelNuestro .fondoModal").hide();

	$('#botonListo').show();

	$("#login").hide();
	$("#campoBatalla").show();
}

function salirPartida () {
	$("#login").show();
	$("#campoBatalla").hide();
	$('#error').empty();
	finpartida = true;
	desconectar();
	conectar();
}

function listo (event) {
	if ($('#naves .nave').length > 0)	{
		$('#error').html('Posiiones todas las naves para iniciar la partida.');
	}
	else {
		enviarPosicionesNaves();
		$('#botonListo').hide();
		$("#panelNuestro .fondoModal").show();
	}
}

function turno() {
	$("#panelJuego .fondoModal").hide();
	$('#error').text("Su Turno.");
}

function finTurno () {
	$("#panelJuego .fondoModal").show();
	$('#error').html('Esperando Turno.');
}

function error(errorarg) {
	if (!finpartida){
		$('#login').hide();
		$("#campoBatalla").hide();
		$('#error').html("ERROR : <br/><br/>"+errorarg);
	}
}

function desconexion () {
	error("Desconectado");
}
//---------------Creación de Elementos-------------------------------------
function borrarPartidas () {
	$("#tablaPartidas").empty();
}


function anadirPartidaALista(partida){
	var arrayPartida = partida.split("@");

	var celda = $('<td></td>');
	
	if (arrayPartida[1] == "partidaNoDisponible"){
		var estado = $('<div class="partida">Cerrada</div>');
		celda.append(estado);
	}
	else{
		var boton = $('<input type="button" class="join button" value="Unirse"/>');
		boton.click(joinPartida);
		celda.append(boton);
	}

	var fila = $('<tr>'
					+'<td><div class="partida">'+arrayPartida[0]+'</div></td>'
				+'</tr>');
	fila.append(celda);

	$("#tablaPartidas").append(fila);
	boton = celda = fila = arrayPartida = estado = null;
}

function crearTablero(contenedor,tamano, nuestro){
	$(contenedor).find('table').remove();
	var tabla = $("<table></table>");
	contenedor.append(tabla);
	for (var i = 0; i < tamano; i++) {
		var fila = $("<tr></tr>");

		tabla.append(fila);
		
		for (var j = 0; j < tamano; j++) {
			var celda = $("<td></td>");
			fila.append(celda);
			// Inicializamos nuestro tablero
			if (nuestro){
				celda.addClass("celda");
				celda.droppable({
      					drop: dropNave,
      					accept: ".nave"
    			});

			}// inicializamos el tablero de juego
			else{
				celda.addClass("objetivo");
				$(celda).click(disparo);
			}
		}
	}
	tabla = null;
}

function anadirNaves(contenedor,numero){
	$(contenedor).find('.nave').remove();
	
	var tipoNave = $("input:radio[name='ship']:checked").val()

	for (var i = 0; i < numero; i++) {
		var nave = $("<div class='nave "+tipoNave+"'></div>").draggable(
      { addClasses: false,
			  stop: function( event, ui ) {
				  ui.helper.css("left","0");
				  ui.helper.css("top","0");
			  }
		  });

		nave.mouseup(function(){
			$(this).css("left","0");
			$(this).css("top","0");
		});
		contenedor.append(nave);
		nave = null;
	}
	tipoNave = null;
}

function enviarPosicionesNaves () {
	var col = null;
	var row = null;

  	$("#panelNuestro .nave").each(function () {
  		var celda = $(this).parent();
		col = celda.parent().children().index(celda);
  		row = celda.parent().parent().children().index(celda.parent());

  		posicionarNave(nick,nombrePartida,col,row);
  	});

  	col = null;
	row = null;
}

//----------------------------Batalla------------------------------------

function dropNave(event, ui){
	var nave = ui.draggable;
	if ($(this).find(".nave").length == 0){
		$(this).append(nave);
	}
	nave = null;
}

function disparo(event){
	$("#panelJuego .fondoModal").show();
	var celda = $(this);

	var col = celda.parent().children().index(celda);
  	var row = celda.parent().parent().children().index(celda.parent());

  	wsdisparo(nick,nombrePartida,col,row);
  	$('#error').html('Esperando Turno.');
  	col = null;
	row = null;
}

function recibirDisparo(naves,x,y,alcanzado,nuestro){

	if(nuestro){
		var celdaJuego = $("#panelJuego table").find('tr:eq('+y+') td:eq('+x+')');
		if(naves>0){
			celdaJuego.addClass("alcanzado").addClass("explosion");
			celdaExplosion2 = celdaJuego;
			setTimeout(apagarExplosion, 800);
			celdaJuego.text(naves+"");
		}
		else
			celdaJuego.addClass("disparado");
		celdaJuego.unbind( "click" );
	}
	else{
		var celdaNuestro  = $("#panelNuestro table").find('tr:eq('+y+') td:eq('+x+')');
		if(alcanzado){
			celdaNuestro.addClass("alcanzado").addClass("explosion");
			celdaExplosion1 = celdaNuestro;
			setTimeout(apagarExplosion, 800);
			celdaNuestro.find(".nave").remove();
			celdaNuestro.text(naves+"");
		}
		else
			celdaNuestro.addClass("disparado");
	}

	celdaNuestro = null;
	celdaJuego = null;	
}

function apagarExplosion(control){
	$(celdaExplosion1).removeClass("explosion");
	$(celdaExplosion2).removeClass("explosion");
}


function ganador () {
	$('#error').html("campeon");
	$("#panelJuego .fondoModal").show();
	$("#panelNuestro .fondoModal").show();
	finpartida = true;
	desconectar();
}

function perdedor () {
	$('#error').html("perdedor");
	$("#panelJuego .fondoModal").show();
	$("#panelNuestro .fondoModal").show();
	finpartida = true;
	desconectar();
}

//-----------Validación-----------------------------------------------------

function validarNombre(event) {
	    var ew = event.which;
    if(48 <= ew && ew <= 57)
        return true;
    if(65 <= ew && ew <= 90)
        return true;
    if(97 <= ew && ew <= 122)
        return true;
    if(ew == 8 || ew == 9 || ew == 13)
        return true;
    return false;
}
