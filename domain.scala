import scala.util.matching.Regex 
class Mode
{
  val modeType = "none"
  def getType():String = {
    return this.modeType
  }
  def processCommand(command:String, window:Window):Mode = {
    return this
  }
}

class NormalMode extends Mode
{
  override val modeType = "normal"
  var pendingCommands:String = ""
  var availableCommands:List[String] = List("diW", "x", "i", "gg")

  override def processCommand(command:String, window:Window):Mode = {
    this.pendingCommands += command
    if (isComplete(this.pendingCommands)) {
      val newCommand = this.pendingCommands
      this.pendingCommands = ""
      return this.execute(newCommand, window)
    } else {
      if (isPossible(this.pendingCommands) == false) {
        this.pendingCommands = ""
      }
    }
    return this
  }

  def execute(command:String, window:Window):Mode = {
    if (command == "i") {
      return new InsertMode
    }
    if (command == "x") {
      window.delete()
    }
    if (command  == "diW") {
      val previousWhiteSpace = window.buffer.getPreviousWhitespace(window.cursorPosition)
      val nextWhiteSpace = window.buffer.getNextWhitespace(window.cursorPosition)
      //window.delete(previousWhiteSpace, nextWhiteSpace)
      true
    }
    return this
  }

  def isComplete(checkedCommand:String):Boolean = {
    return this.availableCommands.contains(checkedCommand)
  }

  def isPossible(checkedCommand:String):Boolean = {
    var returnVal = false
    this.availableCommands.foreach(command => 
      if (command.matches("^" + checkedCommand + ".*"))
        returnVal = true
    )
    return returnVal
  }
}

class InsertMode extends Mode
{
  override val modeType = "insert"

  override def processCommand(command:String, window:Window):Mode = {
    if (command == "<Esc>") {
      return new NormalMode
    }
    window.insert(command)
    return this
  }
}

class Editor
{
    var windows:List[Window] = List()
    var buffers:List[Buffer] = List()
    var bufferNumber = 0
    var mode:Mode = new NormalMode

    def addBuffer() {
      this.buffers = this.buffers ::: List(new Buffer(this.bufferNumber))
      this.bufferNumber += 1
    }

    def getBuffers():List[Buffer] = {
      return this.buffers
    }

    def addWindow() {
      this.addBuffer()
      var newWindow = new Window
      newWindow.setBuffer(this.getBuffers().last)
      this.windows = this.windows ::: List(newWindow)
    }

    def processCommand(command:String) {
      val returnedMode = this.mode.processCommand(command, this.getCurrentWindow())
      if (returnedMode.getType() != "none") {
        this.mode = returnedMode
      }
    }

    def getWindowContents():String = {
      return this.getCurrentWindow().getContents()
    }

    def getCurrentWindow():Window = {
      return this.windows(0)
    }
}
class Window
{
    var cursorPosition:List[Int] = List(0,0)
    var buffer:Buffer = new Buffer(0)

    def setBuffer(newBuffer:Buffer) {
      this.buffer = newBuffer
    }

    def getContents():String = {
        return this.buffer.getContents()
    }

    def insert(string:String) {
        this.buffer.insert(cursorPosition, string)
        this.cursorPosition = List(this.cursorPosition.head, this.cursorPosition.last + string.length)
    }

    def getCursorPosition():List[Int] = {
      return this.cursorPosition
    }

    def delete() {
      this.delete(this.cursorPosition)
    }

    def delete(range:List[Int]) {
      this.buffer.delete(range.last, range.head)
      val cursorIsAtEndOfLine = cursorPosition.last == this.buffer.getLineLength(cursorPosition.head)
      if (cursorIsAtEndOfLine) {
        this.cursorPosition = List(this.cursorPosition.head, this.cursorPosition.last - 1)
      }
    }
}
class Buffer(val bufferNumber:Int)
{
    var lines:Array[String] = Array("")

    def getContents():String = {
      var linesAsText = ""
      this.lines.foreach(line => linesAsText += line)
      return linesAsText
    }

    def getLineLength(lineNumber:Int):Int = {
      return this.lines(lineNumber).length + 1
    }

    def insert(position:List[Int], insertedString:String) {
      this.lines(position.head) += insertedString
      //var line = this.lines(position.head)
      //if (position.last != 0) {
        //val earlierPart = line.substring(0, position.last)
        //val laterPart = line.substring(position.last, line.length)
        //if (earlierPart.length > 0 && laterPart.length > 0) {
          //line = earlierPart + insertedString + laterPart
        //}
      //} else {
        //line += insertedString
      //}
    }

    def delete(x:Int, y:Int) {
      val firstPart = this.lines(y).substring(0, x)
      val lastPart = this.lines(y).substring(x, this.lines(y).length)
      this.lines(y) = firstPart.substring(0, firstPart.length - 1) + lastPart
    }

    def getPreviousWhitespace(position:List[Int]):Int = {
      //var stringBeforePosition = this.lines(position.head).substring(0, position.last)
      //return stringBeforePosition.indexOf(" ")
      0
    }

    def getNextWhitespace(position:List[Int]):Int = {
      //var stringAfterPosition = 
        //this.lines(position.head).substring(position.last, this.lines(position.head).length)
      //return stringAfterPosition.indexOf(" ")
      0
    }
}

// Tests

var editor = new Editor()
// Basic editing
editor.addWindow()
editor.processCommand("i")
editor.processCommand("a")
editor.processCommand("z")
editor.processCommand("j")
editor.processCommand("i")
editor.processCommand("<Esc>")
editor.processCommand("i") 

println("Should be azji: " + editor.getWindowContents())
println("Should be List(0, 4): " + editor.getCurrentWindow().getCursorPosition())

editor.processCommand("<Esc>")
editor.processCommand("x")

println("Should be azj: " + editor.getWindowContents())

// Buffers
println("Should be List(0, 3): " + editor.getCurrentWindow().getCursorPosition())
println(editor.getBuffers())
editor.addBuffer()
println(editor.getBuffers())
println(editor.getCurrentWindow().buffer)

editor.processCommand("d")
editor.processCommand("i")
editor.processCommand("W")
