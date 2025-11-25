# Paradigma-Game
  Este proyecto es un videojuego estilo Action RPG (Roguelike) ejecutado en la terminal. El jugador controla a un pistolero que debe derrotar a un jefe final ("La gran computadora") en un combate en tiempo real por turnos.

  El juego destaca por su implementación puramente funcional de la lógica de estado utilizando la Mónada State de Haskell, separando completamente la lógica del juego de la interfaz de usuario.

## Para compilar y ejecutar este juego necesitas:

  GHC (Glasgow Haskell Compiler).

  Make (Herramienta de construcción).

  Paquete random de Haskell.

## Instrucciones de Compilación y Ejecución

  El proyecto incluye un Makefile para facilitar la compilación.

  Compilar el juego:

    make


  Esto generará un ejecutable llamado boss_battle.

  Ejecutar el juego:

    make run


  Limpiar archivos temporales:

    make clean

## Cómo Jugar

  El juego utiliza un sistema de entrada responsivo (no es necesario presionar Enter).

 ### Controles

    Tecla   Acción

    W       Mover Arriba

    A       Mover Izquierda

    S       Mover Abajo

    D       Mover Derecha

    E       Disparar (Siempre hacia arriba)

    Q       Salir del juego

## Interfaz del Mapa

    @ : Tú (El Pistolero).

    _ : El cuerpo del Jefe.

    X : Punto Débil del Jefe (¡Dispara aquí para daño crítico!).

    ! : Zona de peligro (Rayo cargando). ¡Muévete de ahí!

    + : Poción Roja (+1 Vida).

    ~ : Poción Verde (Veneno, -1 Vida).

    ^ : Mejora de Daño (+1 al ataque, acumulable).

    La gran computadora: 
    ______________________
    ______________________
    ||      ____        ||
    ||     /    \       ||
    ||    | O  O |      ||
    ||     \ ^^ /       ||
    ||      ||||        ||
    ______________________
    ______________________


## Reglas del Combate

  Objetivo: Reducir la vida del Jefe a 0 antes de perder tus 3 vidas.

  Turnos: Cada vez que te mueves o disparas, el jefe reacciona inmediatamente.

  Ataques del Jefe:

    El jefe carga rayos verticales (!).

    Si te quedas en un ! al siguiente turno, pierdes una vida.

  Punto Crítico (X): El punto débil cambia de posición cada 4 turnos o al recibir un disparo.

  Fase de Furia: Cuando al jefe le queda 30 HP o menos, entra en estado de furia y lanzará 7 rayos simultáneos en lugar de 5.

## Diseño Técnico: La Mónada State

  Estructura del Proyecto

  Game.hs: Contiene la definición del GameState y todas las reglas del juego.

  main.hs: Maneja el renderizado en terminal, la lectura del teclado y el bucle principal.

## Implementación de la Mónada State

  Se definió un tipo propio para encapsular el estado:

    type Game a = State GameState a


  El GameState almacena todo lo que cambia en el tiempo:

    Posición y vida del jugador.

    Mapa y ubicación de ítems.

    Estado del jefe (HP, temporizadores, ataques).

    Semilla aleatoria (StdGen).
