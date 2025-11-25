# Makefile

# Nombre del ejecutable final
TARGET = boss_battle

# Compilador de Haskell
GHC = ghc

# Flags de compilación. ¡Añadimos -package random!
GHCFLAGS = --make -o $(TARGET) -package random

# Archivo principal que contiene 'main'
MAIN = main.hs

# Regla por defecto: compilar el juego
all: $(TARGET)

$(TARGET): $(MAIN) Game.hs
	@echo "Compilando la batalla contra el jefe..."
	$(GHC) $(GHCFLAGS) $(MAIN)

# Regla para ejecutar el juego
run: $(TARGET)
	@echo "Iniciando..."
	./$(TARGET)

# Regla para limpiar los archivos compilados
clean:
	@echo "Limpiando archivos compilados..."
	rm -f *.o *.hi $(TARGET)

.PHONY: all run clean