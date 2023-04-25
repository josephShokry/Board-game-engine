class Point{
    x;
    y;
    constructor(x, y)
    {
        this.x = x;
        this.y = y;
    }
    
    isEqual(otherPoint){
        if (this.x == otherPoint.x && this.x == otherPoint.y) return true;
        return false;
    }
}
class ChessMove
{
    point1;
    constructor(point1)
    {
        this.point1 = point1;
    }
}

class Piece{
    constructor(){}
    getAsci(){}
}
class QueenPiece extends Piece{
    constructor(){
        super()
    }
    getAsci(){
        return '♛';
    }
}

class EmptyPiece extends Piece{
    constructor(){
        super()
    }
    isValidMove(board, point){
        for(let i = 0;i<8;i++){
            if(board[point.x][i] instanceof QueenPiece ||
                board[i][point.y] instanceof QueenPiece)
                    return false;
        }
        let counter = 1;
        for(let i = point.x-1;i>=0;i--){
            if(board[i][point.y - counter] instanceof QueenPiece ||
                board[i][point.y + counter] instanceof QueenPiece)
                    return false;
            counter++;
        }
        counter = 1;
        for(let i = point.x+1;i<8;i++){
            if(board[i][point.y - counter] instanceof QueenPiece ||
                board[i][point.y + counter] instanceof QueenPiece)
                    return false;
            counter++;
        }
        return true;
    }
    getAsci(){
        return " ";
    }
}

class QueensEngine extends Engine{
    constructor(){
        super(1,8,8);
        this.controller = new QueensController(this.board);
        this.drawer = new QueensDrawer(this.board);
    }
    initializeBoardPieces(){
        for (var i = 0; i < 8; i++)
            for (var j = 0; j < 8; j++)
                this.board[i][j] = new EmptyPiece();
    }
}

class QueensController extends Controller{
    constructor(board){
        var numOfPlayers = 1;
        super(numOfPlayers, board);
    }
    validateMove(move){
        var point1 = move;
        if (Math.min(point1.x, point1.y) < 0 || Math.max(point1.x, point1.y) >= 8)
            return false;

        var piece = this.board[point1.x][point1.y];

        if(piece instanceof QueenPiece) return true;

        if (piece.isValidMove(this.board, point1))
            return true;
        return false;
    }
    convertInputToMove(moveString){
        var list = moveString.split(" ");
        var col1 = parseInt(list[0]);
        var row1 = parseInt(list[1]);

        row1--;
        col1--;

        var point1 = new Point(row1, col1);
        return point1;
    }

    makeBoardChangeAfterMove(move){
        let point = move
        if(this.board[point.x][point.y] instanceof EmptyPiece){
            this.board[point.x][point.y] = new QueenPiece();
        }
        else this.board[point.x][point.y] = new EmptyPiece();
    }
}
class QueensDrawer extends Drawer{
    constructor(board){
        super(board);
    }
}
var myEngine = new QueensEngine();