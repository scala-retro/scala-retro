package com.github.acout.scalaretro

import scala.meta._
import java.nio.file.Path
import java.io.{File, FileWriter}

object Main extends App{

    def getMermaidFromClass(c: Defn.Class) = {
        val tparams = c.tparams.map(x => x.name).mkString(",")
        val hasTParams = !tparams.isEmpty
        val hasVals = !c.templ.stats.filter(_.isInstanceOf[Decl.Val]).isEmpty
        val hasDefs = !c.templ.stats.filter(_.isInstanceOf[Defn.Def]).isEmpty
        val classDefString = 
            if(hasTParams){
                "class " + c.name/* + "~" + tparams + "~"*/
            }else{
                "class " + c.name
            } + {
                if(hasVals || hasDefs){
                    "{\n"
                }else{
                    "\n"
                }
            } +
            c.templ.stats.map(x => x match {
                case v: Decl.Val => "\t" + v.pats(0).asInstanceOf[Pat.Var].name + ": " + v.decltpe + "\n"
                case f: Defn.Def => "\t" + f.name + f.paramss.map("(" + _.map(p => p.name + ": " + p.decltpe.get).mkString(", ") + ")").mkString("") + "\n"
                case _ => ""
            }).mkString("") + {
                if(hasVals || hasDefs){
                    "}\n"
                }else{
                    "\n"
                }
            } +
            c.templ.inits.map(x => x.tpe).map(x => {
                val indexOfGeneric = x.toString.indexOf("[")
                val xx = if(indexOfGeneric >= 0) x.toString.slice(0, indexOfGeneric) else x.toString
                c.name + " --|> " + xx.toString.replace("[", "(").replace("]", ")").replace(" ", "").replace("~~", "~") + "\n"
            }).mkString("")
        classDefString + "\n"
    }

    def getMermaidFromTrait(t: Defn.Trait) = {
        val tparams = t.tparams.map(x => x.name).mkString(",")
        val hasTParams = !tparams.isEmpty
        val hasVals = !t.templ.stats.filter(_.isInstanceOf[Decl.Val]).isEmpty
        val hasDefs = !t.templ.stats.filter(_.isInstanceOf[Defn.Def]).isEmpty
        val classDefString = 
            if(hasTParams){
                "class " + t.name/* + "~" + tparams + "~"*/
            }else{
                "class " + t.name
            } +
            {
                if(hasVals || hasDefs){
                    "{\n"
                }else{
                    "\n"
                } 
            } +
            t.templ.stats.map(x => x match {
                case v: Decl.Val => "\t" + v.pats(0).asInstanceOf[Pat.Var].name + ": " + v.decltpe + "\n"
                case f: Defn.Def => "\t" + f.name + f.paramss.map("(" + _.map(p => p.name + ": " + p.decltpe.get).mkString(", ") + ")").mkString("") + "\n"
                case _ => ""
            }).mkString("") + {
                if(hasVals || hasDefs){
                    "}\n"
                }else{
                    "\n"
                }
            } +
            t.templ.inits.map(x => x.tpe).map(x => {
                val indexOfGeneric = x.toString.indexOf("[")
                val xx = if(indexOfGeneric >= 0) x.toString.slice(0, indexOfGeneric) else x.toString
                t.name + " --|> " + xx.toString.replace("[", "(").replace("]", ")").replace(" ", "").replace("~~", "~") + "\n"
            }).mkString("")
        classDefString + "\n"
    }

    if(args.length > 0){
        println(args(0))
    }

    /*val tree = q"""
        import java.util.UUID
        case class A[X](x: Int) extends B{
            val i: Int
            def f(a: Boolean)(b: Int): Int = ???
        }
        class B extends C
    """*/

    def getMermaidFromTree(tree: Tree) = {
        tree match {
            case c: Defn.Class => {
                getMermaidFromClass(c)
            }
            case t: Term.Block => {
                t.children.map(x => x match {
                case c: Defn.Class => {
                        getMermaidFromClass(c)
                    }
                    case tt: Defn.Trait => {
                        getMermaidFromTrait(tt)
                    }
                    case _ => ""
                }).mkString("")
            }
            case tt: Defn.Trait => {
                getMermaidFromTrait(tt)
            }
            case p: Pkg => {
                p.children.map(x => x match {
                case c: Defn.Class => {
                        getMermaidFromClass(c)
                    }
                    case tt: Defn.Trait => {
                        getMermaidFromTrait(tt)
                    }
                    case _ => ""
                }).mkString("")
            }
        }
    }

    def getTree(f: File) = {
        val path = f.toPath
        val bytes = java.nio.file.Files.readAllBytes(path)
        val text = new String(bytes, "UTF-8")
        val input = Input.VirtualFile(path.toString, text)
        input.parse[Source].get.children.head
    }

    def getAllScalaFiles(p: Path): List[File] = {
        val d = p.toFile
        if (!d.exists || !d.isDirectory) {
            List[File]()
        } else {
            d.listFiles.filter(_.isFile).toList ++ d.listFiles.filter(_.isDirectory).flatMap(d => getAllScalaFiles(d.toPath))
        }
    }

    val path = java.nio.file.Paths.get("..", "unsupervise", "c4e-arch1", "core", "src", "main", "scala")
    //val path = java.nio.file.Paths.get("src", "main", "resources")
    val files = getAllScalaFiles(path)

    val tokens = new ScalaTokenizer().tokenize(files)
    new MermaidClassDiagramWriter(new FileWriter(new File("output.md"))).write(tokens)

    /*println("classDiagram")
    println(files.map(f => {
        val tree = getTree(f)
        getMermaidFromTree(tree)
    }).mkString(""))*/

}