erlWars_ang
===========

Repositorio para la práctica de AS del CAIT de la UDC.

Esta es la práctica del grupo 7 de la asignatura AS (Arquitectura Software)
De la UDC del curso CAIT.

Esta aplicación consiste en un servidor web de juego online, que permite jugar
al tradicional juego de "hundir la flota" desde un navegador web.

La aplicación cuenta de dos partes principales :

  - Cliente : Desarrollado en HTML + js, se encarga de interactuar con el
              usuario y de realizar la comunicación con el servidor.

  - Servidor: Desarrollado en Erlang, se encarga de gestionar las peticiones
              recibidas desde los clientes, estas peticiones se reciben a 
              traves de un websocket y son principalmente mensajes compuestos
              que el servidor interpreta, y en función de los cuales genera
              una respuesta hacia todos los clientes implicados en la operación
              asociada al mensaje.

Para compilar y ejecutar este proyecto ser requiere disponer en el PATH de las
aplicaciones 'make' y 'GNU git'.

Compilación :

  - CLEAN : Aceder al directorio erlwars y ejecutar el comando 'make clean-all', 
            esta acción desencadenara la ejecución de la limpieza del proyecto
            borrando todos los archivos generados por el proceso de compilación,
            construcción de test y construcción de documentación.

  - BUILD : Aceder al directorio erlwars y ejecutar el comando 'make', esta acción 
            desencadenara el proceso de compilación el cual se descargara las últimas
            versiones de las dependencias y compilara y construira el proyecto.

Documentación :

  - Aceder al directorio erlwars y ejecutar el comando 'make docs', esta acción
    desencadera el proceso de construcción de la documentación el cual creara
    un directorio docs el cual contendra una versión de la misma que se podrá
    consultar desde un navegador web o un visor html.

Test :

  - Aceder al directorio erlwars y ejecutar el comando 'make tests', esta acción
    desencadenara el proceso de creación de los test. Como nuestra aplicación
    utiliza el servidor web cowboy hemos decidido utilizar Common Test para
    realizar las pruebas, las cuales principalmente están agrupadas en una
    única Suite la cual implementa pruebas de caja negra de los módulos implicados
    en la aplicación y cubre la funcionalidad contenida en cada uno de ellos.

Ejecución :

  - Para ejecutar la práctica una vez compilada se debe de ejecutar el script
    'run.sh' el cual desplegara el servidor de juego. Para levantar clientes
    para realizar partidas ejecutar 'cliente.sh N', este script levantara 1
    cliente en un navegdor
    web firefox.

  - Para detener el servidor ejecutar en la terminal Contrl + c, Contrl +q.

  Consideraciones para la ejecución :

    - El servidor de juego esta preconfigurado para desplegarse sobre la
      ruta [http://localhost:8080](http://localhost:8080).

    - Se puede modificar este parámetro en el archivo WebSocket.js ubicado
      en [erlwars/priv/js], modificando la siguiente línea,
     [wsHost = "ws://127.0.0.1:8080/websocket";].


Componentes del grupo 7 :

  - Alejandro Pernas Pan <alejandro.pernas@udc.es>
  - Laura Otero Méndez <inslom01@udc.es﻿>
  - Javier Toja Alamancos <insjta00@udc.es﻿>
  - Sergio Cores Acha <sergio.cores@udc.es>


