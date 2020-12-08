package com.github.acout.scalaretro.core.writer

import com.github.acout.scalaretro.core.token._
import java.io.{File, FileWriter}

object MermaidClassDiagramWriter {
    def write(tokens: List[Token], fw: FileWriter): Unit = new MermaidClassDiagramWriter(fw) {
        write(tokens)
        close
    }

    def write(tokens: List[Token], f: File): Unit = write(tokens, new FileWriter(f))

    def write(tokens: List[Token], fn: String): Unit = write(tokens, new FileWriter(fn))
}

class MermaidClassDiagramWriter(fw: FileWriter, escapeHtml: Boolean = false) {

    def write(tokens: List[Token], includeHeader: Boolean = true): Unit = {
        if(includeHeader) fw.write("classDiagram\n")
        tokens.foreach(x => write(x))
    }

    def write(token: Token): Unit = {
        token match {
            case ClassToken(name, attributes, methods, _, classType) => {
                fw.write("class " + name)
                if(!attributes.isEmpty || !methods.isEmpty || classType.repr.nonEmpty){
                    fw.write("{\n")
                }
                if (escapeHtml)
                  fw.write("\t " + classType.repr.replace("<", "&lt;").replace(">", "&gt;") + "\n")
                else
                  fw.write("\t " + classType.repr + "\n")
                attributes.foreach(a => {
                    fw.write("\t" + s" ${a.encapsulation.repr}" + cleanString(a.name) + ": " + cleanString(a.t) + "\n")
                })
                methods.foreach(m => {
                    fw.write("\t" + s" ${m.encapsulation.repr}" + cleanString(m.name) + "(" + m.params.map(_.map(p => cleanString(p.name) + ": " + cleanString(p.t)).mkString(", ")).mkString(")(") + "): " + cleanString(m.returnType) + "\n")
                })
                if(!attributes.isEmpty || !methods.isEmpty || classType.repr.nonEmpty){
                    fw.write("}")
                }
            }
            case InheritanceToken(from, to) => {
                //TODO: Deal with generics ? How to manage nested generics not supported without loosing first level generics ?
                fw.write(from + " --|> " + cleanString(to))
            }
            case AssociationToken(from, to) => {
                //TODO: Deal with generics ? How to manage nested generics not supported without loosing first level generics ?
                fw.write(from + " --> " + cleanString/*Association*/(to))
            }
            case DependencyToken(from, to) => {
                //TODO: Deal with generics ? How to manage nested generics not supported without loosing first level generics ?
                fw.write(from + " ..> " + cleanString/*Association*/(to))
            }
        }
        fw.write("\n")
    }

    def close = {
        fw.close
    }

    def cleanString(str: String): String = {
        val sstr = if(str.contains("[")){
            val firstIndex = str.indexOf("[")
            val lastIndex = str.lastIndexOf("]")
            str.slice(0, firstIndex + 1) + str.slice(firstIndex + 1, lastIndex).replace("[", if (escapeHtml) "&lt;" else "<").replace("]", if (escapeHtml) "&gt;" else ">") + str.slice(lastIndex, str.length)
        }else{
            str
        }
        sstr/*.replace("[", "(").replace("]", ")")*/.replace(" ", "").replace("[", "~").replace("]", "~").replace(".", "_").replace("*", "").replaceAll("Some\\((.*)\\)", "$1")
    }

}