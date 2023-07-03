open System

type Cell = Alive | Dead

type Grid = Cell[,]

let createGrid(rows: int, cols: int) =
    Array2D.create<Cell> rows cols

let initializeGrid(grid: Grid) =
    let random = Random()
    let numRows = Array2D.length1 grid
    let numCols = Array2D.length2 grid
    for row in 0 .. numRows - 1 do
        for col in 0 .. numCols - 1 do
            let cell = if random.Next(2) = 0 then Cell.Alive else Cell.Dead
            grid.[row, col] <- cell

let getNeighborCount(grid: Grid, row: int, col: int) =
    let numRows = Array2D.length1 grid
    let numCols = Array2D.length2 grid
    let mutable count = 0
    for i in -1 .. 1 do
        for j in -1 .. 1 do
            let neighborRow = (row + i + numRows) % numRows
            let neighborCol = (col + j + numCols) % numCols
            if grid.[neighborRow, neighborCol] = Cell.Alive then
                count <- count + 1
    count

let updateCellState(grid: Grid, row: int, col: int) =
    let neighborCount = getNeighborCount grid row col
    match grid.[row, col] with
    | Cell.Alive when neighborCount < 2 || neighborCount > 3 -> Cell.Dead
    | Cell.Dead when neighborCount = 3 -> Cell.Alive
    | cell -> cell

let updateGrid(grid: Grid) =
    let numRows = Array2D.length1 grid
    let numCols = Array2D.length2 grid
    let newGrid = createGrid numRows numCols
    for row in 0 .. numRows - 1 do
        for col in 0 .. numCols - 1 do
            let newCell = updateCellState grid row col
            newGrid.[row, col] <- newCell
    newGrid

let printGrid(grid: Grid) =
    let numRows = Array2D.length1 grid
    let numCols = Array2D.length2 grid
    for row in 0 .. numRows - 1 do
        for col in 0 .. numCols - 1 do
            let cell = grid.[row, col]
            let cellChar = match cell with
                           | Cell.Alive -> "X"
                           | Cell.Dead -> " "
            printf "%s " cellChar
        printfn ""

let simulateGameOfLife(numRows: int, numCols: int, iterations: int) =
    let grid = createGrid numRows numCols
    initializeGrid grid
    for i in 0 .. iterations - 1 do
        printGrid grid
        grid <- updateGrid grid
        Console.ReadLine() |> ignore

// Usage example
simulateGameOfLife 10 10 10
