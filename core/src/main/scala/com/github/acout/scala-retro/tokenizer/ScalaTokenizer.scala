package com.github.acout.scalaretro.core.tokenizer

import com.github.acout.scalaretro.core.token._

import scala.meta.{Decl, Defn, Input, Mod, Pat, Pkg, Source, Term}
import java.nio.file.Path
import java.io.File

import scala.meta.Mod.{Case, ValParam, VarParam}

class ScalaTokenizer(outputAttributes: Boolean = true, outputMethods: Boolean = true, outputInheritancies: Boolean = true,
                     outputAssociations: Boolean = true, outputDependencies: Boolean = true, outputPackages: Boolean = true, outputObjects: Boolean = true) extends Tokenizer {

    @annotation.tailrec
    private def getEncapsulation(mods: List[Mod]): Encapsulation = mods match {
        case Nil => Public
        case h :: _ if h.isInstanceOf[Mod.Private] => Private
        case h :: _ if h.isInstanceOf[Mod.Protected] => Protected
        case _ => getEncapsulation(mods.tail)
    }

    def tokenize(lf: List[File]): List[Token] = {
        val tokens = lf.flatMap(f => tokenize(f))
        val className = tokens.collect{case ClassToken(name, _, _, _, _) => name}
        tokens.flatMap{
            case DependencyToken(source, target) => className.filter(name => name==target || s"""(\\[$name(\\[.*\\])?\\])|($name\\[.*\\])""".r.findFirstIn(target).isDefined).map(newTarget => DependencyToken(source, newTarget))
            case AssociationToken(source, target) => className.filter(name => name==target || s"""(\\[$name(\\[.*\\])?\\])|($name\\[.*\\])""".r.findFirstIn(target).isDefined).map(newTarget => DependencyToken(source, newTarget))
            case t @ _ => List(t)
        }.distinct
    }

    def tokenize(f: File): List[Token] = {

        val trees = getTree(f)

        trees.flatMap(tree => {
            tree match {
                case c: Defn.Class => {
                    tokenizeClass(c)
                }
                case o: Defn.Object if outputObjects => {
                    tokenizeObject(o)
                }
                case t: Term.Block => {
                    t.children.flatMap(x => x match {
                        case c: Defn.Class => {
                            tokenizeClass(c)
                        }
                        case tt: Defn.Trait => {
                            tokenizeTrait(tt)
                        }
                        case o: Defn.Object if outputObjects => {
                            tokenizeObject(o)
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
                            tokenizeClass(c, if (outputPackages) Some(p.ref.toString()) else None)
                        }
                    case tt: Defn.Trait => {
                            tokenizeTrait(tt, if (outputPackages) Some(p.ref.toString()) else None)
                        }
                    case o: Defn.Object if outputObjects => {
                        tokenizeObject(o, if (outputPackages) Some(p.ref.toString()) else None)
                    }
                        case _ => List[Token]()
                    })
                }
                case _ => List[Token]()
            }
        })

    }

    def getTree(f: File) = {
        val path = f.toPath
        val bytes = java.nio.file.Files.readAllBytes(path)
        val text = new String(bytes, "UTF-8")
        val input = Input.VirtualFile(path.toString, text)
        //val treeChildren = input.parse[Source].get.children
        //if(treeChildren.isEmpty) Tree else treeChildren.head
        input.parse[Source].get.children
    }

    def tokenizeClass(c: Defn.Class, pkg: Option[String] = None): List[Token] = {
        val tparams = c.tparams.map(x => x.name).mkString(",")
        val hasTParams = !tparams.isEmpty
        val hasVals = !c.templ.stats.filter(_.isInstanceOf[Decl.Val]).isEmpty
        val hasDefs = !c.templ.stats.filter(_.isInstanceOf[Defn.Def]).isEmpty
        val constructorAttributes = if (c.mods.exists(_.isInstanceOf[Case]))
            c.ctor.paramss.flatten.map(p => Attribute(p.name.toString(), p.decltpe.getOrElse("").toString, getEncapsulation(p.mods)))
        else c.ctor.paramss.flatten.collect{
            case p if p.mods.exists(_.isInstanceOf[VarParam]) || p.mods.exists(_.isInstanceOf[ValParam]) => Attribute(p.name.toString(), p.decltpe.getOrElse("").toString, getEncapsulation(p.mods))
        }

        // Add support for all types of attributes
        val attributes = c.templ.stats.collect {
            case x @ (_: Decl.Val) if x.pats.head.isInstanceOf[Pat.Var] => Attribute(x.pats.head.asInstanceOf[Pat.Var].name.toString, x.decltpe.toString(), getEncapsulation(x.mods))
            case x @ (_: Defn.Val) if x.pats.head.isInstanceOf[Pat.Var] => Attribute(x.pats.head.asInstanceOf[Pat.Var].name.toString, x.decltpe.getOrElse("").toString, getEncapsulation(x.mods))
            case x @ (_: Decl.Var) if x.pats.head.isInstanceOf[Pat.Var] => Attribute(x.pats.head.asInstanceOf[Pat.Var].name.toString, x.decltpe.toString, getEncapsulation(x.mods))
            case x @ (_: Defn.Var) if x.pats.head.isInstanceOf[Pat.Var] => Attribute(x.pats.head.asInstanceOf[Pat.Var].name.toString, x.decltpe.getOrElse("").toString, getEncapsulation(x.mods))
        }
        /*
        val attributes = c.templ.stats.filter(_.isInstanceOf[Decl.Val]).map(x => {
            val v = x.asInstanceOf[Decl.Val]
            Attribute(v.pats(0).asInstanceOf[Pat.Var].name.toString, v.decltpe.toString, getEncapsulation(v.mods))
        })
        */
        val methods = c.templ.stats.filter(_.isInstanceOf[Defn.Def]).map(x => {
            val f = x.asInstanceOf[Defn.Def]
            val params = f.paramss.map(plist => {
                plist.map(p => Parameter(p.name.toString, p.decltpe.get.toString))
            })
            Method(f.name.toString, params, f.decltpe.toString, getEncapsulation(f.mods))
        })
        val classToken: ClassToken = 
            if(hasTParams){
                ClassToken(c.name.toString/* + "~" + tparams + "~"*/, if(outputAttributes) constructorAttributes ++ attributes else List[Attribute](), if(outputMethods) methods else List[Method](), pkg, ClassT)
            }else{
                ClassToken(c.name.toString, if(outputAttributes) constructorAttributes ++ attributes else List[Attribute](), if(outputMethods) methods else List[Method](), pkg, ClassT)
            }
        val inheritanceTokens = c.templ.inits.map(x => x.tpe).map(x => {
            val indexOfGeneric = x.toString.indexOf("[")
            val xx = if(indexOfGeneric >= 0) x.toString.slice(0, indexOfGeneric) else x.toString
            InheritanceToken(c.name.toString, xx.toString.replace("[", "(").replace("]", ")").replace(" ", "").replace("~~", "~"))
        })
        val associationTokens = (constructorAttributes ::: attributes).map(a => {
            AssociationToken(c.name.toString, a.t)
        }).filter(_.target.matches("^[a-zA-Z][^=<]*"))
        val dependencyTokens = methods.flatMap(m => {
            m.params.flatMap(plist => {
                plist.map(p => DependencyToken(c.name.toString, p.t))
            }) ++ List(DependencyToken(c.name.toString, m.returnType)) 
        }).filter(_.target.matches("^[a-zA-Z][^=<]*"))
        List(classToken) ++ 
            {if(outputInheritancies) inheritanceTokens else List[Token]()} ++
            {if(outputAssociations) associationTokens else List[Token]()} ++ 
            {if(outputDependencies) dependencyTokens else List[Token]()}
    }

    def tokenizeTrait(c: Defn.Trait, pkg: Option[String] = None): List[Token] = {
        val tparams = c.tparams.map(x => x.name).mkString(",")
        val hasTParams = !tparams.isEmpty
        val hasVals = !c.templ.stats.filter(_.isInstanceOf[Decl.Val]).isEmpty
        val hasDefs = !c.templ.stats.filter(_.isInstanceOf[Defn.Def]).isEmpty
        // Add support for all types of attributes
        val attributes = c.templ.stats.collect {
            case x @ (_: Decl.Val) if x.pats.head.isInstanceOf[Pat.Var] => Attribute(x.pats.head.asInstanceOf[Pat.Var].name.toString, x.decltpe.toString(), getEncapsulation(x.mods))
            case x @ (_: Defn.Val) if x.pats.head.isInstanceOf[Pat.Var] => Attribute(x.pats.head.asInstanceOf[Pat.Var].name.toString, x.decltpe.getOrElse("").toString, getEncapsulation(x.mods))
            case x @ (_: Decl.Var) if x.pats.head.isInstanceOf[Pat.Var] => Attribute(x.pats.head.asInstanceOf[Pat.Var].name.toString, x.decltpe.toString, getEncapsulation(x.mods))
            case x @ (_: Defn.Var) if x.pats.head.isInstanceOf[Pat.Var] => Attribute(x.pats.head.asInstanceOf[Pat.Var].name.toString, x.decltpe.getOrElse("").toString, getEncapsulation(x.mods))
        }
        /*
        val attributes = c.templ.stats.filter(_.isInstanceOf[Decl.Val]).map(x => {
            val v = x.asInstanceOf[Decl.Val]
            Attribute(v.pats(0).asInstanceOf[Pat.Var].name.toString, v.decltpe.toString, getEncapsulation(v.mods))
        })
        */
        val methods = c.templ.stats.filter(_.isInstanceOf[Defn.Def]).map(x => {
            val f = x.asInstanceOf[Defn.Def]
            val params = f.paramss.map(plist => {
                plist.map(p => Parameter(p.name.toString, p.decltpe.get.toString))
            })
            Method(f.name.toString, params, f.decltpe.toString, getEncapsulation(f.mods))
        })
        val classToken: ClassToken = 
            if(hasTParams){
                ClassToken(c.name.toString/* + "~" + tparams + "~"*/, if(outputAttributes) attributes else List[Attribute](), if(outputMethods) methods else List[Method](), pkg, TraitT)
            }else{
                ClassToken(c.name.toString, if(outputAttributes) attributes else List[Attribute](), if(outputMethods) methods else List[Method](), pkg, TraitT)
            }
        val inheritanceTokens = c.templ.inits.map(x => x.tpe).map(x => {
            val indexOfGeneric = x.toString.indexOf("[")
            val xx = if(indexOfGeneric >= 0) x.toString.slice(0, indexOfGeneric) else x.toString
            InheritanceToken(c.name.toString, xx.toString.replace("[", "(").replace("]", ")").replace(" ", "").replace("~~", "~"))
        })
        val associationTokens = attributes.map(a => {
            AssociationToken(c.name.toString, a.t)
        }).filter(_.target.matches("^[a-zA-Z][^=<]*"))
        val dependencyTokens = methods.flatMap(m => {
            m.params.flatMap(plist => {
                plist.map(p => DependencyToken(c.name.toString, p.t))
            }) ++ List(DependencyToken(c.name.toString, m.returnType)) 
        }).filter(_.target.matches("^[a-zA-Z][^=<]*"))
        List(classToken) ++ 
            {if(outputInheritancies) inheritanceTokens else List[Token]()} ++
            {if(outputAssociations) associationTokens else List[Token]()} ++ 
            {if(outputDependencies) dependencyTokens else List[Token]()}
    }

    def tokenizeObject(c: Defn.Object, pkg: Option[String] = None): List[Token] = {
        val hasVals = !c.templ.stats.filter(_.isInstanceOf[Decl.Val]).isEmpty
        val hasDefs = !c.templ.stats.filter(_.isInstanceOf[Defn.Def]).isEmpty
        // Add support for all types of attributes
        val attributes = c.templ.stats.collect {
            case x @ (_: Decl.Val) if x.pats.head.isInstanceOf[Pat.Var] => Attribute(x.pats.head.asInstanceOf[Pat.Var].name.toString, x.decltpe.toString(), getEncapsulation(x.mods))
            case x @ (_: Defn.Val) if x.pats.head.isInstanceOf[Pat.Var] => Attribute(x.pats.head.asInstanceOf[Pat.Var].name.toString, x.decltpe.getOrElse("").toString, getEncapsulation(x.mods))
            case x @ (_: Decl.Var) if x.pats.head.isInstanceOf[Pat.Var] => Attribute(x.pats.head.asInstanceOf[Pat.Var].name.toString, x.decltpe.toString, getEncapsulation(x.mods))
            case x @ (_: Defn.Var) if x.pats.head.isInstanceOf[Pat.Var] => Attribute(x.pats.head.asInstanceOf[Pat.Var].name.toString, x.decltpe.getOrElse("").toString, getEncapsulation(x.mods))
        }
        /*
        val attributes = c.templ.stats.filter(_.isInstanceOf[Decl.Val]).map(x => {
            val v = x.asInstanceOf[Decl.Val]
            Attribute(v.pats(0).asInstanceOf[Pat.Var].name.toString, v.decltpe.toString, getEncapsulation(v.mods))
        })
        */
        val methods = c.templ.stats.filter(_.isInstanceOf[Defn.Def]).map(x => {
            val f = x.asInstanceOf[Defn.Def]
            val params = f.paramss.map(plist => {
                plist.map(p => Parameter(p.name.toString, p.decltpe.get.toString))
            })
            Method(f.name.toString, params, f.decltpe.toString, getEncapsulation(f.mods))
        })
        val classToken: ClassToken = ClassToken(c.name.toString, if(outputAttributes) attributes else List[Attribute](), if(outputMethods) methods else List[Method](), pkg, ObjectT)
        val inheritanceTokens = c.templ.inits.map(x => x.tpe).map(x => {
            val indexOfGeneric = x.toString.indexOf("[")
            val xx = if(indexOfGeneric >= 0) x.toString.slice(0, indexOfGeneric) else x.toString
            InheritanceToken(c.name.toString, xx.toString.replace("[", "(").replace("]", ")").replace(" ", "").replace("~~", "~"))
        })
        val associationTokens = attributes.map(a => {
            AssociationToken(c.name.toString, a.t)
        }).filter(_.target.matches("^[a-zA-Z][^=<]*"))
        val dependencyTokens = methods.flatMap(m => {
            m.params.flatMap(plist => {
                plist.map(p => DependencyToken(c.name.toString, p.t))
            }) ++ List(DependencyToken(c.name.toString, m.returnType))
        }).filter(_.target.matches("^[a-zA-Z][^=<]*"))
        List(classToken) ++
          {if(outputInheritancies) inheritanceTokens else List[Token]()} ++
          {if(outputAssociations) associationTokens else List[Token]()} ++
          {if(outputDependencies) dependencyTokens else List[Token]()}
    }

}