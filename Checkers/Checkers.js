class CheckersPiece extends Piece{
    constructor(player, asci) {
        super()
        this.player = player
        this.asci = asci
        if(asci == redPiece){
            this.dx = [-1, -1]
            this.dy = [-1, 1]
        }else{
            this.dx = [1, 1]
            this.dy = [-1, 1]
        }
    }
    getAsci(){
        return this.asci
    }
}

class CheckersMove{
    constructor(point1, point2) {
        this.point1 = point1
        this.point2 = point2
    }
}

const blackPiece = '⚫';
const redPiece = '🔴';
const redQueen = '⭕'
const blackQueen = '⚈'
const emptySquare = new CheckersPiece(-1, " ");
const n = 8;

class CheckersEngine extends Engine {
    constructor() {
        super(2, n, n)
        this.paths = new Array(new Array(n))
        for (let i = 0; i < n; i++)
            this.paths[i] = new Array(n)

        // this.controller = new CheckersController(this.board)
        // this.drawer = new CheckersDrawer(this.board, this.boardCSS)

        this.drawer(this.board)
    }
    initializeCssBoard(){
        console.log("TEST");
        let cell1 = new Cell( '#e59110', undefined, undefined, undefined, undefined)
        let cell2 = new Cell( '#ffcfb6', undefined, undefined, undefined, undefined)
        for(let i= 0;i<this.dimx;i++) {
            for (let j = 0; j < this.dimy; j++) {
                if ((i+j) % 2 == 0)
                    this.boardCSS[i][j] = cell1
                else
                    this.boardCSS[i][j] = cell2
            }
        }
        console.log(this.boardCSS)
    }
    initializeBoardDimensions() {
        this.board =  new Array(n).fill().map(_ => new Array(n).fill(emptySquare))
    }

    initializeBoardPieces(){
        for (let i = 1; i < n; i+=2)
        {
            this.board[0][i] = new CheckersPiece(1, blackPiece)
            this.board[1][i-1] = new CheckersPiece(1, blackPiece)
            this.board[2][i] = new CheckersPiece(1, blackPiece)

            this.board[5][i-1] = new CheckersPiece(0, redPiece)
            this.board[6][i] = new CheckersPiece(0, redPiece)
            this.board[7][i-1] = new CheckersPiece(0, redPiece)
        }
    }

    controller(move, board){
        return this.validateMove(move, board)
    }

    drawer(board)
    {
        this.initializeCss()
        console.log(board)
        for (let i = 0; i < this.dimy; i++) {
            for (let j = 0; j < this.dimx; j++) {
                let temp = this.convertIndicesToHTMLDivs(i, j)
                temp.textContent = ''
                let node = document.createTextNode(board[this.dimy - i - 1][j].getAsci())
                temp.appendChild(node);
            }
        }
    }
    initializeCss(){
        console.log(this.boardCSS)
        for (let i = 0; i < this.boardCSS.length; i++)
            for (let j = 0; j < this.boardCSS[i].length; j++){
                this.initializeCssCell(this.boardCSS[i][j], this.convertIndicesToHTMLDivs(i, j))
            }
    }
    initializeCssCell(cell, divElement) {
        divElement.className = cell.className
        divElement.style.background = cell.color
        divElement.style.width = cell.width + 'px'
        divElement.style.height = cell.height + 'px'
        divElement.style.fontSize = cell.font + 'px'
        divElement.style.border = cell.border + 'px solid'
        if (cell.shape == "circle")
            divElement.style.borderRadius = '50%';
    }

    convertIndicesToHTMLDivs(i, j) {
        let colName = String.fromCharCode('a'.charCodeAt(0) + j)
        let rowNum = i+1
        return document.getElementById(colName + rowNum);
    }
    validateMove(indexedMoves, board){
        let firstCell = indexedMoves.point1
        let secondCell = indexedMoves.point2
        let currentPiece = board[firstCell.x][firstCell.y]

        if(indexedMoves.length > 2)
            return false
        this.curre
        if(currentPiece === emptySquare || this.board[secondCell.x][secondCell.y] != emptySquare
            || currentPiece.player != this.currentplayer)
            return false

        let nullPoint = new Point(-1, -1)
        for (let i = 0; i < n; i++)
            this.paths[i].fill(nullPoint)

        // get all valid moves for this piece
        let validMoves = this.getValidMoves(firstCell)
        console.log("valid moves: ", validMoves)

        // check if this move is in the list of valid moves
        let flag = false
        validMoves.forEach(element => {
            if(element.x == secondCell.x && element.y == secondCell.y)
                flag = true
        })
        return flag
    }
    getValidMoves(cell, paths){
        let validCells = [];
        this.populateValidCellsArray(cell, 0, cell ,validCells)
        return validCells
    }

    // TODO make queen
    populateValidCellsArray(cell, depth, parent, validCells) {
        let dx = this.board[cell.x][cell.y].dx
        let dy = this.board[cell.x][cell.y].dy
        for (let i = 0; i < dx.length; i++){
            let nx = cell.x + dx[i]
            let ny = cell.y + dy[i]
            if(!this.isValidDimensions(nx, ny))
                continue;
            if(this.board[nx][ny] === emptySquare && depth === 0){
                this.paths[nx][ny] = cell
                validCells.push(new Point(nx, ny))
            }
            else if(this.board[nx][ny] !== emptySquare && this.isValidDimensions(nx+dx[i], ny+dy[i]) &&
                this.board[nx+dx[i]][ny+dy[i]] == emptySquare){
                validCells.push(new Point(nx+dx[i], ny+dy[i]))
                this.paths[nx][ny] = cell
                this.paths[nx+dx[i]][ny+dy[i]] = new Point(nx, ny)
                this.populateValidCellsArray(new Point(nx+dx[i], ny+dy[i]), depth+1, cell, validCells)
            }
        }
    }

    isValidDimensions(x, y){
        return x >= 0 && x < n && y >= 0 && y < n
    }

    makeBoardChangeAfterMove(move) {
        let cell1 = move.point1
        let cell2 = move.point2
        let node = cell2
        while ((node.x != cell1.x || node.y != cell1.y) && node.x != -1){
            console.log("deleted: ", node)
            this.board[node.x][node.y] = emptySquare
            node = this.paths[node.x][node.y]
        }

        this.board[cell2.x][cell2.y] = this.board[cell1.x][cell1.y]
        this.board[cell1.x][cell1.y] = emptySquare
        if(cell2.x == 0 && this.board[cell2.x][cell2.y].asci == redPiece){
            console.log("queen promoted");
            this.board[cell2.x][cell2.y].dx = [-1, -1, 1, 1]
            this.board[cell2.x][cell2.y].dy = [-1, 1, -1, 1]
            this.board[cell2.x][cell2.y].asci = redQueen
        }else if(cell2.x == n-1 && this.board[cell2.x][cell2.y].asci == blackPiece){
            console.log("queen promoted");
            this.board[cell2.x][cell2.y].dx = [-1, -1, 1, 1]
            this.board[cell2.x][cell2.y].dy = [-1, 1, -1, 1]
            this.board[cell2.x][cell2.y].asci = blackQueen
        }
    }
    createGameMoveFromInput(indexedCells){
        return new CheckersMove(indexedCells[0], indexedCells[1])
    }
}

class CheckersDrawer extends Drawer{
    constructor(board, boardCSS) {
        super(board, boardCSS)
    }
}
class CheckersController extends Controller{
    constructor(board) {
        super(2, board)

    }
}
let engine = new CheckersEngine();
