import scala.meta._

object Main extends App{

    val tree = q"""
        class A[X] extends B{val i: Int}
        class B extends C
    """

    println(tree.structure)
    println(tree)

    tree match {
        case c: Defn.Class => {
            c.templ.inits.map(x => x.tpe).foreach(x => {
                println(c.name + " --|> " + x)
            })
        }
        case t: Term.Block => {
            t.children.foreach(x => x match {
              case c: Defn.Class => {
                    val tparams = c.tparams.map(x => x.name).mkString(",")
                    val hasTParams = !tparams.isEmpty
                    val hasVals = !c.templ.stats.filter(_.isInstanceOf[Decl.Val]).isEmpty
                    if(hasTParams){
                        print("class " + c.name + "~" + tparams + "~")
                    }else{
                        print("class " + c.name)
                    }
                    if(hasVals){
                        println("{")
                    }else{
                        println("")
                    }
                    c.templ.stats.foreach(x => x match {
                        case v: Decl.Val => println("\t" + v.pats(0).asInstanceOf[Pat.Var].name + ": " + v.decltpe)
                    })
                    if(hasVals){
                        println("}")
                    }
                    c.templ.inits.map(x => x.tpe).foreach(x => {
                        println(c.name + " --|> " + x)
                    })
                }
            })
        }
    }

}