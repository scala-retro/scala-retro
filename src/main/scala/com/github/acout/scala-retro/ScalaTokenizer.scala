package com.github.acout.scalaretro

import scala.meta._
import java.nio.file.Path
import java.io.File

sealed trait Token
case class ClassToken(name: String, attributes: List[String], methods: List[String]) extends Token
case class InheritanceToken(child: String, parent: String) extends Token
case class AssociationToken(source: String, target: String) extends Token

trait Tokenizer {

    def tokenize(f: File): List[Token]

}

class ScalaTokenizer {

    def tokenize(lf: List[File]): List[Token] = lf.flatMap(f => tokenize(f))

    def tokenize(f: File): List[Token] = {

        val tree = getTree(f)

        tree match {
            case c: Defn.Class => {
                tokenizeClass(c)
            }
            case t: Term.Block => {
                t.children.flatMap(x => x match {
                    case c: Defn.Class => {
                        tokenizeClass(c)
                    }
                    case tt: Defn.Trait => {
                        tokenizeTrait(tt)
                    }
                    case _ => List[Token]()
                })
            }
            case tt: Defn.Trait => {
                tokenizeTrait(tt)
            }
            case p: Pkg => {
                p.children.flatMap(x => x match {
                case c: Defn.Class => {
                        tokenizeClass(c)
                    }
                    case tt: Defn.Trait => {
                        tokenizeTrait(tt)
                    }
                    case _ => List[Token]()
                })
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

    def tokenizeClass(c: Defn.Class): List[Token] = {
        val tparams = c.tparams.map(x => x.name).mkString(",")
        val hasTParams = !tparams.isEmpty
        val hasVals = !c.templ.stats.filter(_.isInstanceOf[Decl.Val]).isEmpty
        val hasDefs = !c.templ.stats.filter(_.isInstanceOf[Defn.Def]).isEmpty
        val attributes = c.templ.stats.filter(_.isInstanceOf[Decl.Val]).map(x => {
            val v = x.asInstanceOf[Decl.Val]
            v.pats(0).asInstanceOf[Pat.Var].name + ": " + v.decltpe
        })
        val methods = c.templ.stats.filter(_.isInstanceOf[Defn.Def]).map(x => {
            val f = x.asInstanceOf[Defn.Def]
            f.name + f.paramss.map("(" + _.map(p => p.name + ": " + p.decltpe.get).mkString(", ") + ")").mkString("")
        })
        val classToken: ClassToken = 
            if(hasTParams){
                ClassToken(c.name.toString/* + "~" + tparams + "~"*/, attributes, methods)
            }else{
                ClassToken(c.name.toString, attributes, methods)
            }
        val inheritanceTokens = c.templ.inits.map(x => x.tpe).map(x => {
            val indexOfGeneric = x.toString.indexOf("[")
            val xx = if(indexOfGeneric >= 0) x.toString.slice(0, indexOfGeneric) else x.toString
            InheritanceToken(c.name.toString, xx.toString.replace("[", "(").replace("]", ")").replace(" ", "").replace("~~", "~"))
        })
        List(classToken) ++ inheritanceTokens
    }

    def tokenizeTrait(t: Defn.Trait): List[Token] = {
        val tparams = t.tparams.map(x => x.name).mkString(",")
        val hasTParams = !tparams.isEmpty
        val hasVals = !t.templ.stats.filter(_.isInstanceOf[Decl.Val]).isEmpty
        val hasDefs = !t.templ.stats.filter(_.isInstanceOf[Defn.Def]).isEmpty
        val attributes = t.templ.stats.filter(_.isInstanceOf[Decl.Val]).map(x => {
            val v = x.asInstanceOf[Decl.Val]
            v.pats(0).asInstanceOf[Pat.Var].name + ": " + v.decltpe
        })
        val methods = t.templ.stats.filter(_.isInstanceOf[Defn.Def]).map(x => {
            val f = x.asInstanceOf[Defn.Def]
            f.name + f.paramss.map("(" + _.map(p => p.name + ": " + p.decltpe.get).mkString(", ") + ")").mkString("")
        })
        val classToken: ClassToken = 
            if(hasTParams){
                ClassToken(t.name.toString/* + "~" + tparams + "~"*/, attributes, methods)
            }else{
                ClassToken(t.name.toString, attributes, methods)
            }
        val inheritanceTokens = t.templ.inits.map(x => x.tpe).map(x => {
            val indexOfGeneric = x.toString.indexOf("[")
            val xx = if(indexOfGeneric >= 0) x.toString.slice(0, indexOfGeneric) else x.toString
            InheritanceToken(t.name.toString, xx.toString.replace("[", "(").replace("]", ")").replace(" ", "").replace("~~", "~"))
        })
        List(classToken) ++ inheritanceTokens
    }

}