data ArvBin elemento = Nulo
    | No elemento (ArvBin elemento) (ArvBin elemento)


travessia :: ArvBin elemento -> [elemento]
travessia Nulo = []
-- Travessia em ordem
travessia (No x esq dir) = (travessia esq) ++ [x] ++ (travessia dir)
-- Travessia pré ordem
-- travessia (No x esq dir) = [x] ++ (travessia esq) ++ (travessia dir)
-- Travessia pós ordem
-- travessia (No x esq dir) = (travessia esq) ++ (travessia dir) ++ [x] 

-- preOrdem :: ArvBin elemento -> [elemento]
-- preOrdem Nulo = []
-- preOrdem (No x esq dir) = (preOrdem esq) ++ [x] ++ (preOrdem dir)

-- emOrdem  :: ArvBin elemento -> [elemento]
-- emOrdem Nulo = []
-- emOrdem (No x esq dir) = (emOrdem esq) ++ [x] ++ (emOrdem dir)