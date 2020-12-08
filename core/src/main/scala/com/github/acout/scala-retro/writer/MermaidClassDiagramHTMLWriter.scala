package com.github.acout.scalaretro.core.writer

import java.io.{File, FileWriter}

import com.github.acout.scalaretro.core.token.Token

object MermaidClassDiagramHTMLWriter {
  private val htmlTemplateHeader: String = s"""<html>
                       |  <body>
                       |    %s
                       |    <script>mermaid.initialize({startOnLoad:true});</script>
                       |
                       |    <div class="mermaid">
                       """.stripMargin

  private val htmlTemplateFooter: String = """    </div>
                                             |  </body>
                                             |</html>""".stripMargin

  private def htmlTemplateHeaderDist: String = htmlTemplateHeader.format("""<script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>""")
  private def htmlTemplateHeaderLocal: String = htmlTemplateHeader.format("<script>" + scala.io.Source.fromInputStream(getClass.getResourceAsStream("/mermaid.min.js")).getLines.mkString("\n") + "</script>")

  // Avoid ambiguous method overloading with default parameters
  def write(tokens: List[Token], fw: FileWriter, local: Boolean): Unit = new MermaidClassDiagramHTMLWriter(fw, local) {
    write(tokens)
    close
  }
  def write(tokens: List[Token], fw: FileWriter): Unit = write(tokens, fw, local = true)

  def write(tokens: List[Token], f: File, local: Boolean): Unit = write(tokens, new FileWriter(f), local)
  def write(tokens: List[Token], f: File): Unit = write(tokens, new FileWriter(f), local = true)

  def write(tokens: List[Token], fn: String, local: Boolean): Unit = write(tokens, new FileWriter(fn), local)
  def write(tokens: List[Token], fn: String): Unit = write(tokens, new FileWriter(fn), local = true)
}

class MermaidClassDiagramHTMLWriter(fw: FileWriter, local: Boolean = true) extends MermaidClassDiagramWriter(fw) {

  fw.write(if (local) MermaidClassDiagramHTMLWriter.htmlTemplateHeaderLocal else MermaidClassDiagramHTMLWriter.htmlTemplateHeaderDist)

  override def close: Unit = {
    fw.write(MermaidClassDiagramHTMLWriter.htmlTemplateFooter)
    super.close
  }
}
