import scala.meta._

object Main extends App{

    def getMermaidFromClass(c: Defn.Class) = {
        val tparams = c.tparams.map(x => x.name).mkString(",")
        val hasTParams = !tparams.isEmpty
        val hasVals = !c.templ.stats.filter(_.isInstanceOf[Decl.Val]).isEmpty
        val hasDefs = !c.templ.stats.filter(_.isInstanceOf[Defn.Def]).isEmpty
        if(hasTParams){
            print("class " + c.name + "~" + tparams + "~")
        }else{
            print("class " + c.name)
        }
        if(hasVals || hasDefs){
            println("{")
        }else{
            println("")
        }
        c.templ.stats.foreach(x => x match {
            case v: Decl.Val => println("\t" + v.pats(0).asInstanceOf[Pat.Var].name + ": " + v.decltpe)
            case f: Defn.Def => println("\t" + f.name + f.paramss.map("(" + _.map(p => p.name + ": " + p.decltpe.get).mkString(", ") + ")").mkString(""))
            case _ => {}
        })
        if(hasVals || hasDefs){
            println("}")
        }
        c.templ.inits.map(x => x.tpe).foreach(x => {
            println(c.name + " --|> " + x)
        })
    }

    def getMermaidFromTrait(t: Defn.Trait) = {
        val tparams = t.tparams.map(x => x.name).mkString(",")
        val hasTParams = !tparams.isEmpty
        val hasVals = !t.templ.stats.filter(_.isInstanceOf[Decl.Val]).isEmpty
        val hasDefs = !t.templ.stats.filter(_.isInstanceOf[Defn.Def]).isEmpty
        if(hasTParams){
            print("class " + t.name + "~" + tparams + "~")
        }else{
            print("class " + t.name)
        }
        if(hasVals || hasDefs){
            println("{")
        }else{
            println("")
        }
        t.templ.stats.foreach(x => x match {
            case v: Decl.Val => println("\t" + v.pats(0).asInstanceOf[Pat.Var].name + ": " + v.decltpe)
            case f: Defn.Def => println("\t" + f.name + f.paramss.map("(" + _.map(p => p.name + ": " + p.decltpe.get).mkString(", ") + ")").mkString(""))
            case _ => {}
        })
        if(hasVals || hasDefs){
            println("}")
        }
        t.templ.inits.map(x => x.tpe).foreach(x => {
            println(t.name + " --|> " + x)
        })
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

    val path = java.nio.file.Paths.get("src", "main", "resources", "Instance.scala")
    val bytes = java.nio.file.Files.readAllBytes(path)
    val text = new String(bytes, "UTF-8")
    val input = Input.VirtualFile(path.toString, text)
    val tree = input.parse[Source].get.children.head

    println(tree.structure)

    tree match {
        case c: Defn.Class => {
            getMermaidFromClass(c)
        }
        case t: Term.Block => {
            t.children.foreach(x => x match {
              case c: Defn.Class => {
                    getMermaidFromClass(c)
                }
                case tt: Defn.Trait => {
                    getMermaidFromTrait(tt)
                }
                case _ => {}
            })
        }
        case tt: Defn.Trait => {
            getMermaidFromTrait(tt)
        }
        case p: Pkg => {
            p.children.foreach(x => x match {
              case c: Defn.Class => {
                    getMermaidFromClass(c)
                }
                case tt: Defn.Trait => {
                    getMermaidFromTrait(tt)
                }
                case _ => {}
            })
        }
    }

}