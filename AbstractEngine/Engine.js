class Engine
{
    numOfPlayers;
    dimx;
    dimy;
    board;
    boardCSS;

    constructor(numOfPlayers, dimx, dimy)//, color1, color2)
    {
        this.dimx = dimx;
        this.dimy = dimy;
        this.numOfPlayers = numOfPlayers;
        this.currentplayer = 0;
        this.initializeBoardDimensions();
        this.initializeCSSBoardDimensions();
        this.initializeHTML();
        this.initializeBoardPieces();
        this.initializeCssBoard();
        // initializeHTML(dimx, dimy)//, color1, color2);
    }
    initializeHTML(){
        let container = document.getElementById('board');
        for(let i=0;i<this.dimx;i++){
            for(let j = 0;j<this.dimy;j++){
                let element = document.createElement('div');
                element.id = String.fromCharCode('a'.charCodeAt(0) + j) + (this.dimy - i);
                container.appendChild(element);
            }
        }
    }
    initializeBoardDimensions()
    {
        this.board = new Array(this.dimx);
        for (var i = 0; i < this.dimx; i++)
            this.board[i] = new Array(this.dimy);
    }
    initializeCSSBoardDimensions()
    {
        this.boardCSS = new Array(this.dimx);
        for (var i = 0; i < this.dimx; i++)
            this.boardCSS[i] = new Array(this.dimy);
    }

    initializeBoardPieces()
    {
    }

    initializeCssBoard(){

    }

    takeInputAndMoveToControllerAndDraw()
    {
        let moveString = document.getElementById("nameInput").value;
        var move = this.convertInputToMove(moveString);
        if (this.controller(move, this.board))
        {
            console.log(this.currentplayer)
            this.currentplayer = (this.currentplayer + 1) % this.numOfPlayers;
            this.makeBoardChangeAfterMove(move)
            this.drawer(this.board);
        }
        else
        {
            console.log("Invalid Move");
        }
        this.printPlayerTurnMessage()
    }

    convertInputToMove(playerMove) {
        let cells = playerMove.split(" ")
        let indexedCells = [];
        cells.forEach(element => {
            let col = element.charAt(0).charCodeAt(0) - 'a'.charCodeAt(0)
            let row = this.board.length - parseInt(element.charAt(1));
            //console.log(col, row)
            indexedCells.push(new Point(row, col))
        })
        return this.createGameMoveFromInput(indexedCells)
    }
    controller(){

    }

    drawer(board){

    }
    createGameMoveFromInput() {

    }

    makeBoardChangeAfterMove(move)
    {
    }

    printPlayerTurnMessage()
    {
        console.log("Player " + (parseInt(this.currentplayer) + 1) + " turn");
    }
}

class Piece
{
    constructor()
    {
    }

    getAsci()
    {
    }
}
//
// class Move
// {
//     constructor() {
//     }
// }
