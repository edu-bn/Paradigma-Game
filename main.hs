-- main.hs
import Game
import Control.Monad.State (runState, modify, State)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..), stdin, hSetEcho)
import System.Process (system)
import System.Random (newStdGen)
import Data.List (find)

-- --- Arte ASCII del Jefe ---
bossArt :: String
bossArt = unlines
  [ "______________________"
  , "______________________"
  , "||      ____        ||"
  , "||     /    \\       ||"
  , "||    | O  O |      ||"
  , "||     \\ ^^ /       ||"
  , "||      ||||        ||"
  , "______________________"
  ]

-- --- Funciones de Dibujado ---

tileToChar :: Tile -> Char
tileToChar Floor       = '.'
tileToChar Wall        = '#'
tileToChar PotionRed   = '+'
tileToChar PotionGreen = '~'
tileToChar ItemDmg     = '^'

drawGame :: GameState -> IO ()
drawGame st = do
  system "clear" 
  
  putStrLn "=== BOSS BATTLE: PISTOLERO VS Gran computadora ==="
  putStrLn ""
  
  -- Dibujamos el Jefe
  putStr bossArt
  
  -- Dibujamos el mapa
  putStr (renderMap st)
  
  putStrLn $ "VIDAS: " ++ show (pLives (player st)) ++ 
             " | DMG: " ++ show (pDamage (player st)) ++ 
             " | JEFE HP: " ++ show (bossHp st)
  putStrLn $ "Rondas para mover P. Crítico: " ++ show (critTimer st)
  putStrLn $ "Mensaje: " ++ (message st)
  putStrLn ""
  putStrLn "[CONTROLES]: (W/A/S/D) Mover  |  (E) Disparar  |  (Q) Salir"

-- | Renderiza el mapa
renderMap :: GameState -> String
renderMap st = unlines $ map (mapRow) (zip [0..] (gameMap st))
  where
    (px, py) = pPos (player st)
    critPos = critPoint st
    attackTiles = case bossAttack st of
                    Just coords -> coords
                    Nothing -> []
                    
    mapRow (y, row) = [ getChar (x, y) tile
                      | (x, tile) <- zip [0..] row
                    ]
    
    getChar (x, y) tile
      | (x, y) == (px, py)         = '@' 
      | Just (x, y) == critPos     = 'X'
      | (x, y) `elem` attackTiles  = '!' 
      | y == 0                     = '_'
      | otherwise                  = tileToChar tile

-- --- El Game Loop ---

gameLoop :: GameState -> IO ()
gameLoop currentState = do
  drawGame currentState

  case gameStatus currentState of
    Won -> putStrLn "¡FELICIDADES! ¡HAS DERROTADO AL JEFE!"
    Lost -> putStrLn "GAME OVER... Has sido eliminado."
    
    Running -> do
      input <- getChar
      
      let maybeAction = case input of
            'w' -> Just (movePlayer (0, -1))
            'a' -> Just (movePlayer (-1, 0))
            's' -> Just (movePlayer (0, 1))
            'd' -> Just (movePlayer (1, 0))
            'e' -> Just shoot 
            'q' -> Nothing    
            _   -> Nothing    

      case maybeAction of
        Nothing -> do
            if input == 'q' 
              then putStrLn "Saliendo del juego..."
              else gameLoop currentState 
              
        Just playerAction -> do
          let ((), afterPlayerState) = runState playerAction currentState
          
          if gameStatus afterPlayerState == Won then
             drawGame afterPlayerState >> putStrLn "¡VICTORIA!"
          else do
             let ((), afterBossState) = runState updateBoss afterPlayerState
             gameLoop afterBossState

-- --- Punto de Entrada ---
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hSetBuffering stdout NoBuffering
  
  initialSeed <- newStdGen 
  let state = initialState initialSeed 
  gameLoop state