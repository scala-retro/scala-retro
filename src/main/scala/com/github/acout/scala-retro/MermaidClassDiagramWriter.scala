package com.github.acout.scalaretro

import java.io.FileWriter

class MermaidClassDiagramWriter(fw: FileWriter) {

    def write(tokens: List[Token], includeHeader: Boolean = true): Unit = {
        if(includeHeader) fw.write("classDiagram\n")
        tokens.foreach(x => write(x))
    }

    def write(token: Token): Unit = {
        token match {
            case ClassToken(name, attributes, methods) => {
                fw.write("class " + name)
                if(!attributes.isEmpty || !methods.isEmpty){
                    fw.write("{")
                }
                fw.write("\n")
                attributes.foreach(a => {
                    fw.write("\t" + a + "\n")
                })
                methods.foreach(m => {
                    fw.write("\t" + m + "\n")
                })
                if(!attributes.isEmpty || !methods.isEmpty){
                    fw.write("}")
                }
            }
            case InheritanceToken(from, to) => {
                //TODO: Deal with generics ? How to manage nested generics not supported without loosing first level generics ?
                fw.write(from + " --|> " + to.replace("[", "(").replace("]", ")").replace(" ", "").replace("~~", "~") + "\n")
            }
            case AssociationToken(from, to) => {
                //TODO: Deal with generics ? How to manage nested generics not supported without loosing first level generics ?
                fw.write(from + " ---> " + to.replace("[", "(").replace("]", ")").replace(" ", "").replace("~~", "~") + "\n")
            }
        }
        fw.write("\n")
    }

    def close = {
        fw.close
    }

}