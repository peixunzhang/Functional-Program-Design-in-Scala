package w2
class Pouring(capacity: Vector[Int]) {
// State
  type State = Vector[Int]
  val initialState = capacity map (x => 0)

//Moves
  trait Move {
    def change(state: State): State
  }
  case class Empty(glass: Int) extends Move {
    def change(state: State) = state.updated(glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    def change(state: State) = state.updated(glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State) = {
      val amount = state(from) min (capacity(to) - state(to))
      state
        .updated(from, state(from) - amount)
        .updated(
          to,
          state(
            to
          ) + amount
        )
    }
  }

  val glasses = 0 until capacity.length

  val moves =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <- glasses; to <- glasses if from != to)
        yield Pour(from, to))

// Paths

  class Path(history: List[Move], val endState: State) {
    def extend(move: Move) = new Path(move :: history, move.change(endState))
    override def toString = (history.reverse mkString "  ") + "-->" + endState
  }
  val initialPath = new Path(Nil, initialState)

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Strea0m.empty
    else {
      val more =
        paths.flatMap(path =>
          moves
            .map(move => path.extend(move))
            .filter(next => !(explored.contains(next.endState)))
        )
      paths #:: from(more, explored ++ more.map(_.endState))
    }
  val pathSets = from(Set(initialPath), Set(initialState))
  def solutions(target: Int): Stream[Path] =
    pathSets.flatMap(pathset =>
      pathset.filter(path => path.endState.contains(target))
    )
}
