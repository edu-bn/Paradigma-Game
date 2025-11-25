module Game
  ( GameState(..)
  , Tile(..)
  , Player(..)
  , GameStatus(..)
  , Game
  , movePlayer
  , shoot
  , updateBoss
  , initialState
  , isBossBody
  ) where

import Control.Monad.State
import System.Random (StdGen, randomR)

-- ================================================
-- DEFINICIÓN DE TIPOS
-- ================================================

data Tile = Floor | Wall | PotionRed | PotionGreen | ItemDmg
  deriving (Show, Eq)

data Player = Player
  { pPos :: (Int, Int)
  , pLives :: Int
  , pDamage :: Int 
  } deriving (Show)

data GameState = GameState
  { player :: Player
  , gameMap :: [[Tile]]
  , gameStatus :: GameStatus
  , bossHp :: Int
  , critPoint :: Maybe (Int, Int)
  , critTimer :: Int
  , bossAttack :: Maybe [(Int, Int)]
  , turnCounter :: Int
  , seed :: StdGen
  , message :: String
  }

data GameStatus = Running | Won | Lost
  deriving (Show, Eq)

type Game a = State GameState a

-- ================================================
-- CONSTANTES Y UTILIDADES
-- ================================================

-- Mapa Compacto
initialMap :: [[Tile]]
initialMap =
  [ replicate 22 Floor 
  , [Wall] ++ replicate 20 Floor ++ [Wall]
  , [Wall] ++ replicate 20 Floor ++ [Wall]
  , [Wall] ++ replicate 20 Floor ++ [Wall]
  , replicate 22 Wall 
  ]

mapWidth :: Int
mapWidth = length (head initialMap)

mapHeight :: Int
mapHeight = length initialMap

isBossBody :: (Int, Int) -> Bool
isBossBody (_, y) = y == 0

setTile :: (Int, Int) -> Tile -> [[Tile]] -> [[Tile]]
setTile (x, y) newTile gMap =
  take y gMap ++
  [take x (gMap !! y) ++ [newTile] ++ drop (x + 1) (gMap !! y)] ++
  drop (y + 1) gMap

-- ================================================
-- ACCIONES DEL JUGADOR
-- ================================================

movePlayer :: (Int, Int) -> Game ()
movePlayer (dx, dy) = do
  st <- get
  let p = player st
  let (x, y) = pPos p
  let newPos = (x + dx, y + dy)
  let (nx, ny) = newPos

  if nx < 0 || nx >= mapWidth || ny < 0 || ny >= mapHeight then
    modify (\s -> s { message = "¡Borde!" })
  else do
    let currentMap = gameMap st
    let tile = currentMap !! ny !! nx
    
    case tile of
      Wall -> modify (\s -> s { message = "¡Muro!" })
      PotionRed -> do
        let newMap = setTile newPos Floor currentMap
        modify (\s -> s { player = p { pPos = newPos, pLives = (pLives p) + 1 }
                        , gameMap = newMap
                        , message = "¡Poción Roja! +1 Vida." })
      PotionGreen -> do
        let newMap = setTile newPos Floor currentMap
        let newLives = (pLives p) - 1
        modify (\s -> s { player = p { pPos = newPos, pLives = newLives }
                        , gameMap = newMap
                        , message = "¡Veneno! -1 Vida." })
        checkPlayerDeath
      ItemDmg -> do
        let newMap = setTile newPos Floor currentMap
        modify (\s -> s { player = p { pPos = newPos, pDamage = (pDamage p) + 1 }
                        , gameMap = newMap
                        , message = "¡Arma Mejorada! +1 Daño." })
      Floor -> do
        if isBossBody newPos then
           modify (\s -> s { message = "¡Chocaste con el jefe!" })
        else if gameStatus st == Running then
           modify (\s -> s { player = p { pPos = newPos }, message = "Te mueves..." })
        else return ()
      _ -> return ()

shoot :: Game ()
shoot = do
  st <- get
  let p = player st
  let dmg = pDamage p
  let (px, py) = pPos p
  let bulletPath = take 10 [ (px + 0*i, py + (-1)*i) | i <- [1..] ]
  
  let hit = findHit bulletPath st
  
  case hit of
    Just (pos, "Crit") -> do
      let finalDmg = dmg + 20
      let newHp = (bossHp st) - finalDmg
      spawnNewCritPoint 
      modify (\s -> s { bossHp = newHp, message = "¡CRÍTICO! -" ++ show finalDmg ++ " HP." })
      checkBossDeath
    Just (pos, "Body") -> do
      let newHp = (bossHp st) - dmg
      modify (\s -> s { bossHp = newHp, message = "¡Golpe! -" ++ show dmg ++ " HP." })
      checkBossDeath
    _ -> modify (\s -> s { message = "¡Fallaste!" })

-- ================================================
-- LÓGICA DEL JEFE Y SPAWNING
-- ================================================

updateBoss :: Game ()
updateBoss = do
  st <- get
  if gameStatus st /= Running then return () else do
    
    let turn = (turnCounter st) + 1
    modify (\s -> s { turnCounter = turn })
    
    if turn `mod` 2 == 0 then spawnItems else return ()

    st2 <- get
    -- Lógica del timer de crítico
    if (critTimer st2) <= 0 then do
      spawnNewCritPoint
      modify (\s -> s { message = (message s) ++ " [Punto débil movido]" })
    else
      modify (\s -> s { critTimer = (critTimer s) - 1 })

    st3 <- get
    case bossAttack st3 of
      Nothing -> do
        if bossHp st3 <= 30 then do
          -- FASE DE FURIA: 7 RAYOS
          let (r1, s1) = randomR (1, mapWidth - 2) (seed st3)
          let (r2, s2) = randomR (1, mapWidth - 2) s1
          let (r3, s3) = randomR (1, mapWidth - 2) s2
          let (r4, s4) = randomR (1, mapWidth - 2) s3
          let (r5, s5) = randomR (1, mapWidth - 2) s4
          let (r6, s6) = randomR (1, mapWidth - 2) s5
          let (r7, s7) = randomR (1, mapWidth - 2) s6
          
          let attackCoords = concat [ [(x, y) | y <- [1..mapHeight-2]] | x <- [r1, r2, r3, r4, r5, r6, r7] ]
          modify (\s -> s { bossAttack = Just attackCoords, seed = s7, message = (message s) ++ " || ¡FURIA! ¡7 RAYOS!" })
        else do
          -- FASE NORMAL: 5 
          let (r1, s1) = randomR (1, mapWidth - 2) (seed st3)
          let (r2, s2) = randomR (1, mapWidth - 2) s1
          let (r3, s3) = randomR (1, mapWidth - 2) s2
          let (r4, s4) = randomR (1, mapWidth - 2) s3
          let (r5, s5) = randomR (1, mapWidth - 2) s4
          
          let attackCoords = concat [ [(x, y) | y <- [1..mapHeight-2]] | x <- [r1, r2, r3, r4, r5] ]
          modify (\s -> s { bossAttack = Just attackCoords, seed = s5, message = (message s) ++ " || ¡Jefe prepara 5 rayos!" })
      
      Just coords -> do
        let p = player st3
        if pPos p `elem` coords then do
          modify (\s -> s { player = p { pLives = (pLives p) - 1 }
                          , bossAttack = Nothing
                          , message = (message s) ++ " || ¡Te electrocutaste!" })
          checkPlayerDeath
        else do
          modify (\s -> s { bossAttack = Nothing, message = (message s) ++ " || ¡Esquivaste!" })

spawnItems :: Game ()
spawnItems = do
  trySpawnItem
  trySpawnItem

trySpawnItem :: Game ()
trySpawnItem = do
  st <- get
  let (rx, s1) = randomR (1, mapWidth - 2) (seed st)
  let (ry, s2) = randomR (1, mapHeight - 2) s1 
  let (itemType, s3) = randomR (0, 2 :: Int) s2
  
  let pos = (rx, ry)
  let currentTile = (gameMap st) !! ry !! rx
  
  if ry > 0 && currentTile == Floor && pos /= pPos (player st) then do
    let newItem = case itemType of
                    0 -> PotionRed
                    1 -> PotionGreen
                    _ -> ItemDmg
    let newMap = setTile pos newItem (gameMap st)
    modify (\s -> s { gameMap = newMap, seed = s3 })
  else
    modify (\s -> s { seed = s3 })

spawnNewCritPoint :: Game ()
spawnNewCritPoint = do
  st <- get
  let (randX, newSeed) = randomR (1, mapWidth - 2) (seed st)
  modify (\s -> s { critPoint = Just (randX, 0), critTimer = 4, seed = newSeed })

findHit :: [(Int, Int)] -> GameState -> Maybe ((Int, Int), String)
findHit [] _ = Nothing
findHit (pos:path) st =
  let (x, y) = pos in
  if x < 0 || x >= mapWidth || y < 0 || y >= mapHeight then Nothing
  else
    let tile = (gameMap st) !! y !! x in
    if tile == Wall then Nothing
    else if Just pos == (critPoint st) then Just (pos, "Crit")
    else if isBossBody pos then Just (pos, "Body")
    else findHit path st

checkPlayerDeath :: Game ()
checkPlayerDeath = do
  st <- get
  if pLives (player st) <= 0 then
    modify (\s -> s { gameStatus = Lost, message = (message s) ++ " ¡HAS MUERTO!" })
  else return ()

checkBossDeath :: Game ()
checkBossDeath = do
  st <- get
  if bossHp st <= 0 then
    modify (\s -> s { gameStatus = Won, message = (message s) ++ " ¡VICTORIA!" })
  else return ()

initialState :: StdGen -> GameState
initialState initialSeed = GameState
  { player = Player { pPos = (11, 3), pLives = 3, pDamage = 5 } 
  , gameMap = initialMap
  , gameStatus = Running
  , bossHp = 100
  , critPoint = Nothing
  , critTimer = 0
  , bossAttack = Nothing
  , turnCounter = 0
  , seed = initialSeed
  , message = "¡La gran computadora ha sido infectada, Destrúyela !!!"
  }