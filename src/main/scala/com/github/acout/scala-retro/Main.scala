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
                    c.templ.inits.map(x => x.tpe).foreach(x => {
                        if(hasTParams){
                            println(c.name + "~" + tparams + "~ --|> " + x)
                        }else{
                            println(c.name + " --|> " + x)
                        }
                    })
                    c.templ.stats.foreach(x => x match {
                        case v: Decl.Val => println(c.name + " --> " + v.decltpe + ": " + v.pats(0).asInstanceOf[Pat.Var].name)
                    })
                }
            })
        }
    }

}