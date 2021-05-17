module FourColors where
---------------------------------
--- PRIMERA FASE: ADJACENCIES ---
---------------------------------

--- Tipos de datos
type Zone      = Int
type Row       = [Zone]
type Map       = [Row]
type Adjacency = (Zone,[Zone])

--- Función adjacency
--- Obtiene todas las adyacencias del mapa
adjacencies :: Map -> [Adjacency]
adjacencies (r1:m) = adjAux (r1:m) (adjLeftAux r1 []) -- rmEmp (rmRed (adjAux (r1:m) (adjLeftAux r1 [])))
  where
    -- Bucle principal
    adjAux :: Map -> [Adjacency] -> [Adjacency]
    adjAux (r1:r2:m) acc = adjAux (r2:m) (adjLeftAux r2 (adjUpAux r1 r2 acc))
    adjAux [_] acc = acc
    adjAux _ acc = acc
    -- Busca adyacencias arriba de las celdas
    adjUpAux :: Row -> Row -> [Adjacency] -> [Adjacency]
    adjUpAux (x:xs) (y:ys) acc
      | x /= y    = adjUpAux xs ys (addAdj x y acc)
      | otherwise = adjUpAux xs ys acc
    adjUpAux _ _ acc = acc
    -- Busca adyacencias a la izquierda de las celdas
    adjLeftAux :: Row -> [Adjacency] -> [Adjacency]
    adjLeftAux (r1:r2:rs) acc
      | r1 /= r2  = adjLeftAux (r2:rs) (addAdj r1 r2 acc)
      | otherwise = adjLeftAux (r2:rs) acc
    adjLeftAux _ acc = acc
    addAdj :: Zone -> Zone -> [Adjacency] -> [Adjacency]
    addAdj x y adjs
      | x > y = addAdj1 x y adjs
      | y < x = addAdj1 y x adjs
      | otherwise = addAdj1 y x adjs -- No debería ocurrir
      where
        addAdj1 :: Zone -> Zone -> [Adjacency] -> [Adjacency]
        addAdj1 y x ((z,zs):adjs)
          | y == z    = (z,addAdj2 x zs) : adjs
          | y < z     = (y,[x]) : (z,zs) : adjs
          | otherwise = (z,zs) : addAdj1 y x adjs
          where
            addAdj2 :: Zone -> [Zone] -> [Zone]
            addAdj2 x (z:zs)
              | x == z    = z : zs
              | x < z     = x : z : zs
              | otherwise = z : addAdj2 x zs
            addAdj2 x _ = [x]
        addAdj1 y x _ = [(y,[x])]

---------------------------
--- SEGUNDA FASE: COLOR ---
---------------------------

--- Tipos de datos
data Color    = Red | Green | Blue | Yellow
  deriving (Enum,Eq,Show)
type Solution = [Color]
type Node     = ([Adjacency],Zone,Zone,Solution)

--- Función color
--- Busca una solución para colorear un mapa con un máximo de cuatro colores
---   -Si existe solución, devuelve un String con la solución
---   -Si no existe solución, devuelve un String indicando que no existe
color :: Map -> String
color map = takeFirst (solution map)
  where --- Función takeFirst
        --- Devuelve la solución contenida en el primer elemento de una lista de nodos solución
        --- Si la lista es vacía, devuelve una cadena indicando que no hay soluciones
        takeFirst :: [Node] -> String
        takeFirst []             = "No solution!\n"
        takeFirst ((_,_,_,c):ss) = "Solution: " ++ show c ++ "\n"

--- Función solution
--- Devuelve TODOS los nodos solución del problema
solution :: Map -> [Node]
solution map = bt esSol comp (initialNode map)

--- Función del esquema de Backtracking
--- Esta función aplica el esquema de Backtracking a cualquier problema
bt :: (a -> Bool) -> (a -> [a]) -> a -> [a]
bt esSol comp node
  | esSol node = [node]
  | otherwise  = concatMap (bt esSol comp) (comp node)

--- Función colorList
--- Devuelve la enumeración de todos los elementos del tipo Color
colorList :: [Color]
colorList = [toEnum 0::Color ..]

-- --- Función esSol
-- --- Comprueba si un nodo es solución o no
esSol :: Node -> Bool
esSol (adjs, num, zone, sol) 
  | num == zone && zone == length sol = andAux [1..num] sol adjs
  | otherwise                         = False
  where
    andAux :: [Zone] -> Solution -> [Adjacency] -> Bool
    andAux (z:zs) sol adjs  = null (sameColor z (adjOf adjs z) sol) && andAux zs sol adjs
    andAux _ _ _            = True

--- Función comp
--- Calcula las compleciones del nodo actual coloreando la zona actual con los colores utilizables
comp :: Node -> [Node]
comp (adjs, num, zone, sol) -- = genNodes node (sameColor zone (adjOf adjs zone) sol)
  | num == zone = []
  | otherwise   = genNodes (adjs, num, zone+1, sol) (possibleColors adjs (zone+1) sol)
  where
    genNodes :: Node -> [Color] -> [Node]
    genNodes node@(adjs, num, zone, sol) (c:cs) = (adjs, num, zone, sol ++ [c]) : genNodes node cs
    genNodes _ _                                = []

possibleColors :: [Adjacency] -> Zone -> Solution -> [Color]
possibleColors adjs zone sol = possibleColorsAux adjs zone sol colorList
  where
    possibleColorsAux :: [Adjacency] -> Zone -> Solution -> [Color] -> [Color]
    possibleColorsAux adjs zone sol (c:cs)
      | canBePainted zone sol (adjOf adjs zone) c   = c:possibleColorsAux adjs zone sol cs
      | otherwise                                   = possibleColorsAux adjs zone sol cs
    possibleColorsAux _ _ _ _ = []

canBePainted :: Zone -> Solution -> [Zone] -> Color -> Bool
canBePainted zone sol (a:as) c
  | not(painted a sol) = True
  | colorOf a sol /= c = canBePainted zone sol as c
  | otherwise          = False
canBePainted _ _ _ _ = True

-- Zonas adyacentes a zona
adjOf :: [Adjacency] -> Zone -> [Zone]
adjOf ((a,as):adjs) z
  | a == z              = as ++ adjOf adjs z
  | a /= z && elem z as = a  : adjOf adjs z
  | otherwise           = adjOf adjs z
adjOf _ _ = []

sameColor :: Zone -> [Zone] -> Solution -> [Zone]
sameColor z zs sol = sameColorAux (colorOf z sol) zs sol

sameColorAux :: Color -> [Zone] -> Solution -> [Zone]
sameColorAux c (z:zs) sol
  | painted z sol && colorOf z sol == c = z : sameColorAux c zs sol
  | otherwise                           = sameColorAux c zs sol
sameColorAux _ _ _ = []

-- Color de una zona
colorOf :: Zone -> Solution -> Color
colorOf z (s:ss)
  | z == 1    = s
  | otherwise = colorOf (z-1) ss
colorOf z _   = Red -- Por consistencia, no se debería llegar aquí NUNCA

-- Comprueba si una zona está pintada todavía
painted :: Zone -> Solution -> Bool
painted z (s:sol)
  | z == 1    = True
  | otherwise = painted (z-1) sol
painted _ _ = False

--- Función initialNode
--- Construye el nodo inicial para comenzar el Backtracking
initialNode :: Map -> Node
initialNode m = (adjacencies m, numZonas m, 0, [])
  where
    -- Devuelve el número de zonas de un mapa
    numZonas :: Map -> Zone
    numZonas m = numZonasAux1 m 0
      where
        numZonasAux1 :: Map -> Zone -> Zone
        numZonasAux1 (r:m) x = numZonasAux1 m (numZonasAux2 r x)
          where
            numZonasAux2 :: Row -> Zone -> Zone
            numZonasAux2 (z:r) x
              | z > x     = numZonasAux2 r z
              | otherwise = numZonasAux2 r x
            numZonasAux2 [] x = x
        numZonasAux1 [] x = x