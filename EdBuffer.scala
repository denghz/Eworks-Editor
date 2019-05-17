// EdBuffer.scala
// Copyright (c) 2015 J. M. Spivey
// Amended 2017 by P.G. Jeavons

import java.io.{Reader, Writer, FileReader, FileWriter, IOException}

/** The state of an editing session
  *  (this class is the model part of the MVC architecture) */
class EdBuffer {

  /** The text being edited. */
  private val text = new PlaneText()

  /** The display. */
  private var display: Display = _

  /** Register a display */
  def register(display: Display) { this.display = display }

  // State components that are preserved by undo and redo

  /** Current editing position. */
  private var _point = 0

  // State components that are not restored on undo

  /** File name for saving the text. */
  private var _filename = ""

  /** Dirty flag */
  private var modified = false

  /** Mark the buffer as modified */
  private def setModified() { modified = true }

  /** Test whether the text is modified */
  def isModified: Boolean = modified

  // Display update

  /** Extent that the display is out of date. */
  private var damage = EdBuffer.CLEAN

  /** If damage = REWRITE_LINE, the line that should be rewritten */
  private var damage_line = 0

  /** Note damage to the display. */
  private def noteDamage(rewrite: Boolean) {
    val newdamage =
      if (rewrite) EdBuffer.REWRITE else EdBuffer.REWRITE_LINE
    damage = Math.max(damage, newdamage)
    damage_line = text.getRow(point)
  }

  /** Force a display rewrite at next update */
  def forceRewrite() { noteDamage(true) }

  /** Update display with cursor at current point */
  def update() { update(point, mark) }

  /** Update display with cursor at specified position */
  def update(pos: Int, mk: Int) {
    if (mk != -1)
      display.refresh(damage,
                      text.getRow(pos),
                      text.getColumn(pos),
                      text.getRow(mk),
                      text.getColumn(mk))
    else
      display.refresh(damage, text.getRow(pos), text.getColumn(pos))
    damage = EdBuffer.CLEAN
  }

  /** Initialise display */
  def initDisplay() {
    noteDamage(true)
    update()
  }

  // Accessors

  def point: Int = _point

  def point_=(point: Int) {
    if (damage == EdBuffer.REWRITE_LINE && getRow(point) != damage_line)
      damage = EdBuffer.REWRITE
    _point = point
  }

  def filename: String = _filename

  private def filename_=(filename: String) { _filename = filename }

  // Delegate methods for text

  def charAt(pos: Int): Char = text.charAt(pos)

  def getRow(pos: Int): Int = text.getRow(pos)

  def getColumn(pos: Int): Int = text.getColumn(pos)

  def getPos(row: Int, col: Int): Int = text.getPos(row, col)

  def length: Int = text.length

  def getLineLength(row: Int): Int = text.getLineLength(row)

  def getRange(pos: Int, len: Int): Text.Immutable = text.getRange(pos, len)

  def numLines: Int = text.numLines

  def fetchLine(n: Int, buf: Text) { text.fetchLine(n, buf) }

  def writeFile(out: Writer) { text.writeFile(out) }

  def getString(pos: Int, len: Int): String = text.getString(pos, len)
  // Mutator methods

  /** Delete a character */
  def deleteChar(pos: Int) {
    val ch = text.charAt(pos)
    noteDamage(ch == '\n' || getRow(pos) != getRow(point))
    text.deleteChar(pos)
    if (pos < mark) mark -= 1
    setModified()
  }

  /** Delete a range of characters. */
  def deleteRange(pos: Int, len: Int) {
    noteDamage(true)
    text.deleteRange(pos, len)
    if (pos < mark) mark -= len min (mark - pos)
    setModified()
  }

  /** Insert at current value of point */
  def insert(a: Any) {
    a match {
      case ch: Char  => insert(point, ch)
      case s: String => insert(point, s)
    }
  }

  /** Insert a character at a specified position */
  def insert(pos: Int, ch: Char) {
    noteDamage(ch == '\n' || getRow(pos) != getRow(point))
    text.insert(pos, ch)
    if (pos <= mark) mark += 1
    setModified()
  }

  /** Insert a string */
  def insert(pos: Int, s: String) {
    noteDamage(true)
    text.insert(pos, s)
    if (pos <= mark) mark += s.length
    setModified()
  }

  /** Insert an immutable text. */
  def insert(pos: Int, s: Text.Immutable) {
    noteDamage(true)
    text.insert(pos, s)
    if (pos <= mark) mark += s.length
    setModified()
  }

  /** Insert a Text. */
  def insert(pos: Int, t: Text) {
    noteDamage(true)
    text.insert(pos, t)
    if (pos <= mark) mark += t.length
    setModified()
  }

  def swapChars(back: Int, front: Int): Unit = {
    val (p1, p2) =
      if (back < front) (front, back) else (back, front)
    assert(p1 != length)
    noteDamage(
      getRow(p1) != getRow(p2) || text.charAt(p1) == '\n' || text
        .charAt(p2) == '\n')

    val temp = text.charAt(p1)
    text.deleteChar(p1)
    text.insert(p1, text.charAt(p2))
    text.deleteChar(p2)
    text.insert(p2, temp)
  }

  /** Transpose two Characters */
  def transposeChars(p: Int): Int = {
    if (p == length) throw new IllegalArgumentException("cannot swap with eof")
    if (p == 0)
      throw new IllegalArgumentException("you are already at the start")
    noteDamage(text.charAt(p) == '\n' || text.charAt(p - 1) == '\n')
    val temp = text.charAt(p)
    if (temp != '\n') {
      if (getColumn(p) == 0) {
        var r = getRow(p) - 1
        while (r >= 0 && getLineLength(r) == 1) {
          r -= 1
        }
        if (r == -1)
          throw new IllegalArgumentException("you are already at the start")
        else {
          swapChars(p, getPos(r, getLineLength(r) - 2))
          setModified()
        }
      } else {
        text.deleteChar(p)
        text.insert(p - 1, temp)
        setModified()

      }
      p + 1
    } else {

      var r = getRow(p) + 1
      while (r != numLines && getPos(r, 0) != length && charAt(getPos(r, 0)) == '\n') {
        r += 1
      }

      if (r == numLines || getPos(r, 0) == length)
        throw new IllegalArgumentException("cannot swap with eof")
      swapChars(getPos(r, 0), p - 1)
      setModified()
      getPos(r, 0) + 1
    }

  }

  /** mark a position */
  private var m = -1
  def mark: Int = m
  def mark_=(p: Int): Unit = {
    assert(p <= length)
    m = p
  }

  /** copy a range to a buffer */
  val copyBuf = new Text(20)
  def copyToBuf(back: Int, front: Int): Unit = {
    var (p1, p2) =
      if (back < front) (front, back) else (back, front)
    if (p1 == length) p1 -= 1
    copyBuf.clear()
    copyBuf.insertRange(0, text, p2, p1 - p2 + 1)
  }

  def cutToBuf(back: Int, front: Int): Unit = {
    var (p1, p2) =
      if (back < front) (front, back) else (back, front)
    if (p1 == length) p1 -= 1
    copyToBuf(p1, p2)
    deleteRange(p2, p1 - p2 + 1)
  }

  var searchContent = ""
  def search(): Int = {
    assert(searchContent != null)
    if (searchContent != "") {
      for (i <- point to length - searchContent.length) {
        if (getString(i, searchContent.length) == searchContent)
          return i
      }
      for (i <- 0 to ((point - 1) min length - searchContent.length)) {
        if (getString(i, searchContent.length) == searchContent)
          return i
      }
      throw new NoSuchElementException("Cannot find the string")
    } else throw new NoSuchElementException("Empty input string")
  }

  /** Load a file into the buffer. */
  def loadFile(name: String): Boolean = {
    filename = name

    try {
      val in = new FileReader(name)
      text.clear()
      text.insertFile(0, in)
      in.close()
      modified = false
      noteDamage(true)
      true
    } catch {
      case _: IOException =>
        MiniBuffer.message(display, "Couldn't read file '%s'", name)
        false
    }
  }

  /** Save buffer contents to a file */
  def saveFile(name: String): Boolean = {
    filename = name

    try {
      val out = new FileWriter(name)
      text.writeFile(out)
      out.close()
      modified = false
      true
    } catch {
      case _: IOException =>
        MiniBuffer.message(display, "Couldn't write file '%s'", name)
        false
    }
  }

  /** Make a Memento that records the current state */
  def getState = new Memento()

  /** An immutable record of the editor state at some time.  The state that
    * is recorded consists of just the current point. */
  class Memento {
    private val pt = point
    private val mk = mark

    /** Restore the state when the memento was created */
    def restore() { point = pt; mark = mk }
  }
}

object EdBuffer {

  /** Possible value for damage. */
  val CLEAN = 0
  val REWRITE_LINE = 1
  val REWRITE = 2
}
