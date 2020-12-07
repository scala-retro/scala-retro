package com.github.acout.scalaretro.core.writer

import com.github.acout.scalaretro.core.token.Token

import java.io.FileWriter

private object MermaidClassDiagramHTMLWriter {
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

  def htmlTemplateHeaderDist: String = htmlTemplateHeader.format("""<script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>""")
  def htmlTemplateHeaderLocal: String = htmlTemplateHeader.format("<script>" + scala.io.Source.fromInputStream(getClass.getResourceAsStream("/mermaid.min.js")).getLines.mkString("\n") + "</script>")
}

class MermaidClassDiagramHTMLWriter(fw: FileWriter, local: Boolean = true) extends MermaidClassDiagramWriter(fw) {
  override def write(tokens: List[Token], includeHeader: Boolean): Unit = {
    fw.write(if (local) MermaidClassDiagramHTMLWriter.htmlTemplateHeaderLocal else MermaidClassDiagramHTMLWriter.htmlTemplateHeaderDist)
    super.write(tokens, includeHeader)
    fw.write(MermaidClassDiagramHTMLWriter.htmlTemplateFooter)
  }
}
