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
  override def processCommand(command:String, window:Window):Mode = {
    if (command == "i") {
      return new InsertMode
    }
    if (command == "x") {
      window.delete()
    }
    return this
  }
}

class InsertMode extends Mode
{
  override val modeType = "insert"
  override def processCommand(command:String, window:Window):Mode = {
    if (command == "<Esc>") {
      return new NormalMode
    }
    window.add(command)
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
    def add(string:String) {
        this.buffer.add(string)
        this.cursorPosition = List(this.cursorPosition.head, this.cursorPosition.last + string.length)
    }
    def getCursorPosition():List[Int] = {
      return this.cursorPosition
    }
    def delete() {
      this.delete(this.cursorPosition)
    }
    def delete(position:List[Int]) {
      this.buffer.delete(this.cursorPosition.last, this.cursorPosition.head)
      this.cursorPosition = List(this.cursorPosition.head, this.cursorPosition.last - 1)
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
    def add(string:String) = {
        this.lines(0) = this.lines(0) + string
    }
    def delete(x:Int, y:Int) {
        val firstPart = this.lines(y).substring(0, x)
        val lastPart = this.lines(y).substring(x, this.lines(y).length)
        this.lines(y) = firstPart.substring(0, firstPart.length - 1) + lastPart
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
