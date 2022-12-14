import scala.collection.immutable.TreeSet
import scala.collection.mutable
import scala.io.Source
import scala.language.implicitConversions
import scala.util.Using

object Main {

    // This allows me to treat booleans as integers (0 or 1), which is convenient for some math
    implicit def bool2int(b:Boolean): Int = if (b) 1 else 0

    /**
     * Day 1 part 1: Find the maximum total calories among the elves
     *
     * @param path path to input file
     * @return maximum total calories
     */
    def day1Part1(path: String): Int = {
        Using(Source.fromFile(path)) { data =>                          // load input data file
            data.getLines                                               // for each line in file
                .foldLeft(List.fill(2)(0))(                             // initialize partial solution List to [0,0] (running sum, running maximum)
                    (partial: List[Int], cur: String) =>                // given partial solution and current line
                        if (cur.nonEmpty)                               // if current line is not empty (it's a value)
                            partial.head + cur.toInt :: partial.tail    // update running sum in partial solution
                        else                                            // line is empty (done looking at this elf's inventory
                            0 :: partial.max :: Nil                     // update running maximum, reset running sum to 0
                ).max                                                   // get max between running maximum and final running sum (this is required if the last line of the input is not empty)
        }.get                                                           // get final result
    }


    /**
     * Day 1 part 2: Find the sum of the highest `n` total calories among the elves
     * Note: Problem originally requested highest 3. Abstracted to highest `n` so this solution can also handle part 1!
     *
     * @param path path to input file
     * @param n    number of highest values to get
     * @return sum of highest `n` total calories
     */
    def day1Part2(path: String, n: Int): Int = {
        Using(Source.fromFile(path)) { data =>                                                      // load input data file
            (data.getLines ++ Seq(""))                                                              // for each line in file (adds an extra empty line so the maximums get updated for the final sum)
                 .foldLeft(List.fill(n + 1)(0))(                                                    // initialize partial solution List (first item is running sum, last n items are running maximums)
                    (partial: List[Int], cur: String) =>                                            // given partial solution and current line
                        if (cur.nonEmpty)                                                           // if current line is not empty (it's a value)
                            partial.head + cur.toInt :: partial.tail                                // update running sum in partial solution
                        else                                                                        // line is empty (done looking at this elf's inventory
                            0 :: partial.updated(partial.indexOf(partial.min), partial.head).tail   // change minimum value in List to running sum, reset running sum to 0
                ).sum                                                                               // compute sum of maximums
        }.get                                                                                       // get final result
    }


    /**
     * Day 2 part 1: Find the total score for the rock-paper-scissors competition
     * A/X = Rock; B/Y = Paper; C/X = Scissors
     *
     * @param path path to input file
     * @return total score for competition
     */
    def day2part1(path: String): Int = {
        Using(Source.fromFile(path)) { data =>              // load input data file
            data.getLines                                   // for each line in file
                .foldLeft(0)(                               // initialize partial score to 0
                    (score: Int, cur: String) =>            // given partial score and current line
                        score +                             // add to partial score
                          cur(2) - 'X' + 1 +                // add points for hand choice
                          ((cur(2) - cur(0) - 1) % 3) * 3   // add points for win/draw/loss (wacky math Just Works)
                )
        }.get                                               // get final result
    }


    /**
     * Day 2 part 2: Find the total score for the rock-paper-scissors competition
     * A = Rock; B = Paper; C = Scissors; X = Lose; Y = Draw; Z = Win
     *
     * @param path path to input file
     * @return total score for competition
     */
    def day2part2(path: String): Int = {
     Using(Source.fromFile(path)) { data =>                 // load input data file
            data.getLines                                   // for each line in file
                .foldLeft(0)(                               // initialize partial score to 0
                    (score: Int, cur: String) =>            // given partial score and current line
                        score +                             // add to partial score
                          (cur(2) - 'X') * 3 +              // add points for win/draw/loss
                          (cur(2) + cur(0) - 1) % 3 + 1     // add points for hand choice (wacky math Just Works)
                )
        }.get                                               // get final result
    }


    /**
     * Day 3 part 1: Find the sum of priorities of items in two compartments of the same rucksack
     *
     * @param path path to input file
     * @return sum of priorities
     */
    def day3part1(path: String): Int = {
        Using(Source.fromFile(path)) { data =>                              // load input data file
            data.getLines                                                   // for each line in file
                .foldLeft(0)(                                               // initialize partial sum to 0
                    (sum: Int, rucksack: String) => {                       // given partial sum and current rucksack (line)
                        sum + ((                                            // add priority of shared item (found below) to partial sum
                          rucksack.take(rucksack.length / 2)                // use the left compartment to...
                          intersect                                         // compute the intersection...
                          rucksack.takeRight(rucksack.length / 2)           // with the right compartment
                        )(0)  - 'A' + 'z' - 'a' + 1) % ('z' - 'A' + 1) + 1  // get the item and compute its priority
                    }
                )
        }.get                                                               // get final result
    }


    /**
     * Day 3 part 2: Find the sum of priorities of badges
     * Note: Problem originally stated that badges are shared by 3 adjacent elves. Abstracted to `n` because I can
     *
     * @param path path to input file
     * @param n    number of adjacent elves to find badges in
     * @return sum of priorities of badges
     */
    def day3part2(path: String, n: Int): Int = {
        Using(Source.fromFile(path)) { data =>                              // load input data file
            data.getLines                                                   // for each line in file
                .grouped(n)                                                 // split rucksacks (lines) by desired group size
                .foldLeft(0)(                                               // initialize partial sum to 0
                    (sum: Int, sacks: Seq[String]) => {                     // given partial sum and current group
                        sum + (                                             // add 1 to partial sum
                          sacks.foldLeft(('A' to 'z').mkString)(            // within the group, initialize partial list of shared items
                              (shared: String, rucksack: String) =>         // given the partial list of shared items and a rucksack
                                  shared intersect rucksack                 // compute the intersection of partial shared items and the rucksack
                          )(0) - 'A' + 'z' - 'a' + 1) % ('z' - 'A' + 1) + 1 // get the badge and compute its priority
                    }
                )
        }.get                                                               // get final result
    }


    /**
     * Day 4 part 1: Find the number of assignment pairs where range fully contains the other
     *
     * @param path path to input file
     * @return number of fully contained assignment pairs
     */
    def day4part1(path: String): Int = {
        Using(Source.fromFile(path)) { data =>                      // load input data file
            data.getLines                                           // for each line in file
                .map(x => x.split("[-,]")                           // split string to numeric values
                           .map(_.toInt))                           // convert strings to integers
                .count(values => (                                  // count the number of pairs where...
                  values(0) >= values(2) && values(1) <= values(3)  // first range is entirely within the second range...
                  ) || (                                            // OR...
                  values(0) <= values(2) && values(1) >= values(3)  // second range is entirely within the first range
                  )
                )
        }.get                                                       // get final result
    }


    /**
     * Day 4 part 2: Find the number of assignment pairs that overlap at all
     *
     * @param path path to input file
     * @return number of overlapping assignment pairs
     */
    def day4part2(path: String): Int = {
        Using(Source.fromFile(path)) { data =>                          // load input data file
            data.getLines                                               // for each line in file
                .map(x => x.split("[-,]")                               // split string to numeric values
                           .map(_.toInt))                               // convert strings to integers
                .count(values =>                                        // count the number of pairs where...
                    !(values(0) > values(3) || values(1) < values(2))   // the ranges are NOT exclusive
                )
        }.get                                                           // get final result
    }


    /**
     * Day 5 (both parts): Move crates following specified instructions and find the top crate of the resulting stacks
     *
     * @param path  path to input file
     * @param part2 option to do part 2 instead of part 1
     * @return crates at the top of the stacks
     */
    def day5(path: String, part2: Boolean = false): String = {
        Using(Source.fromFile(path)) { data => {                                    // load input data file
            val parts = data.getLines.span(x => x.nonEmpty)                         // separate initial stack setup from instructions

            val setup = parts._1.toSeq.reverseIterator                              // get and reverse stack setup lines (going top to bottom is easier)
            val stacks = Seq.fill(setup.next.max.asDigit)(mutable.Stack[Char]())    // initialize sequence of crate stacks (also moves the setup one line)

            setup.foreach(line =>                                                   // for each setup line
                 line.grouped(4)                                                    // generate 4-wide groupings (each will have a crate at index 1)
                     .zipWithIndex                                                  // combine crate with associated stack number
                     .foreach(crate =>                                              // for each crate (window)
                         if (crate._1(1) != ' ')                                    // if a crate is present
                             stacks(crate._2).push(crate._1(1))                     // push crate onto corresponding stack
                     )
            )

            parts._2                                                                // using the crate movement instructions
                 .drop(1)                                                           // ignore the first row (its empty)
                 .foreach(line => {                                                 // for each instruction
                     val action = """\d+""".r.findAllIn(line).map(_.toInt).toVector // get (ordered) numeric values in instructions
                     var i = 0                                                      // initialize counter to 0

                     var crates = stacks(action(1) - 1).popWhile(_ => {             // pull crates from the desired stack while...
                         i += 1                                                     // incremented counter...
                         i <= action(0)                                             // ...is less than the desired count
                     })

                     if (part2) crates = crates.reverse                             // if finding part 2 solution, reverse the order of the moved crates

                     stacks(action(2)-1).pushAll(crates)                            // put crates on desired stack
                 })

            stacks.foldLeft("")(                                                    // initialize partial solution to empty string
                (str: String, stack: mutable.Stack[Char]) =>                        // given the partial solution and a stack
                    str + stack.pop                                                 // add the top of the stack to the solution
            )
        }}.get                                                                      // get final result
    }


    /**
     * Day 6 (both parts): Find the number of characters before the packet starts
     *
     * @param path path to input file
     * @param n    size of start-of-packet marker
     * @return number of characters before the packet starts
     */
    def day6(path: String, n: Int): Int = {
        Using(Source.fromFile(path)) { data =>      // load input data file
            data.sliding(n)                         // using a sliding window the size of the start-of-packet marker (n)
                .takeWhile(_.distinct.size != n)    // take elements until the number of distinct characters = n (start-of-packet marker found!)
                .size + n                           // get number of windows before marker was found + size of marker
        }.get                                       // get final result
    }


    /**
     * Day 7 part 1: Find sum of directory sizes
     *
     * @param path path to input file
     * @return sum of directory sizes
     */
    def day7part1(path: String): Int = {
        Using(Source.fromFile(path)) { data =>                                                  // load input data file
            val values = data.getLines                                                          // for each line in file
                             .foldLeft((0, 0, List[Int]()))(                                    // initialize partial solution to tuple (sum of directory sizes < 100000 | current directory size | directory queue)
                                 (partial: (Int, Int, List[Int]), cur: String) => {             // given partial solution and current line
                                     if (cur(0).isDigit) {                                      // if the line is a file (has numeric size)
                                         (partial._1,                                           // sum of directory sizes unchanged
                                          partial._2 + cur.takeWhile(_ != ' ').toInt,           // add file size to current directory size
                                          partial._3)                                           // directory queue unchanged
                                     } else if (cur.take(4) == "$ cd") {                        // directory change
                                         if (cur.substring(5) == "..") {                        // exiting a directory
                                             (partial._1 + partial._2 * (partial._2 < 100000),  // add size of directory we are leaving to total if < 100000
                                              partial._3.head + partial._2,                     // set current directory size to head of directory queue + size of directory we are exiting
                                              partial._3.tail)                                  // remove directory we are going up to from the queue
                                         } else {                                               // not exiting, therefore entering a new directory
                                             (partial._1,                                       // sum of directory sizes unchanged
                                              0,                                                // new directory -> set directory size to 0
                                              partial._2 :: partial._3)                         // add parent directory to queue
                                         }
                                     } else {                                                   // dir or `$ ls`, we can ignore
                                         partial                                                // pass running solution forward
                                     }
                                }
                            )

            (values._2 :: values._3).foldLeft((values._1, 0))(                                  // fold on remaining directories, initialize partial solution to tuple (sum of directory sizes < 100000 | current directory size)
                  (partial: (Int, Int), cur: Int) => {                                          // given partial solution and current observed directory size
                      val dir_size = cur + partial._2                                           // size of this directory = observed size + previous directory size
                      (partial._1 + dir_size * (dir_size < 100000),                             // add size of this directory to total if < 100000
                       dir_size)                                                                // pass up size of this directory
                  })._1                                                                         // get total from tuple output
        }.get                                                                                   // get final result
    }


    /**
     * Day 7 part 2: Find size of smallest directory whose deletion creates a total of 30000000 free [generic memory units] out of 70000000
     *
     * @param path path to input file
     * @return minimum directory size
     */
    def day7part2(path: String): Int = {
        Using(Source.fromFile(path)) { data =>                                                      // load input data file
            val values = data.getLines                                                              // for each line in file
                             .foldLeft((0, 0, List[Int](), TreeSet[Int]()))(                        // initialize partial solution to tuple (total memory usage | current directory size | directory queue | set of directory sizes)
                                 (partial: (Int, Int, List[Int], TreeSet[Int]), cur: String) => {   // given partial solution and current line
                                     if (cur(0).isDigit) {                                          // if the line is a file (has numeric size)
                                         (partial._1 + cur.takeWhile(_ != ' ').toInt,               // add file size to total memory usage
                                          partial._2 + cur.takeWhile(_ != ' ').toInt,               // add file size to current directory's size
                                          partial._3,                                               // directory queue unchanged
                                          partial._4)                                               // directory size set unchanged
                                     } else if (cur.take(4) == "$ cd") {                            // directory change
                                         if (cur.substring(5) == "..") {                            // exiting a directory
                                             (partial._1,                                           // total memory usage unchanged
                                              partial._3.head + partial._2,                         // set current directory size to head of directory queue + size of directory we are exiting
                                              partial._3.tail,                                      // remove directory we are going up to from the queue
                                              partial._4 + partial._2)                              // add current directory size to directory size set
                                         } else {                                                   // not exiting, therefore entering a new directory
                                             (partial._1,                                           // total memory usage unchanged
                                              0,                                                    // new directory -> set directory size to 0
                                              partial._2 :: partial._3,                             // add parent directory to queue
                                              partial._4)                                           // directory size set unchanged
                                         }
                                     } else {                                                       // dir or `$ ls`, we can ignore
                                         partial                                                    // pass running solution forward
                                     }
                                }
                            )

            (values._2 :: values._3).foldLeft((values._4, 0))(                                      // fold on remaining directories, initialize partial solution to tuple (set of directory sizes | current directory size)
                (partial: (TreeSet[Int], Int), cur: Int) => {                                       // given partial solution and current observed directory size
                    val dir_size = cur + partial._2                                                 // size of this directory = observed size + previous directory size
                    (partial._1 + dir_size,                                                         // add size of this directory to the set
                     dir_size)                                                                      // pass up size of this directory
                })._1                                                                               // get directory size set
                .minAfter(values._1 + 30000000 - 70000000)                                          // find the minimum value large enough to give us 30000000 free [generic memory units] out of 70000000
                .get                                                                                // get final result
        }.get
    }


    /**
     * Day 8 part 1: Find the number of trees that are visible from outside the grid
     *
     * @param path path to input file
     * @return number of visible trees
     */
    def day8part1(path: String): Int = {
        def getVisibleInLine(line: Seq[Char]): Seq[Boolean] = {
            var visible = Seq.fill(line.length)(false)                      // initialize list of tree visibilities
            var left_max, right_max = -1                                    // initialize directional maximums
            for (i <- line.indices) {                                       // from 0 to the number of trees in the line
                if (line(i).asDigit > left_max) {                           // if tree i from the left is taller than the left max
                    left_max = line(i).asDigit                              // update left max
                    visible = visible.updated(i, true)                      // set visibility of the tree to true
                }
                if (line(line.length - i - 1).asDigit > right_max) {        // if tree i from the right is taller than the right max
                    right_max = line(line.length - i - 1).asDigit           // update right max
                    visible = visible.updated(line.length - i - 1, true)    // set visibility of the tree to true
                }
            }
            visible                                                         // return visibility list
        }

        Using(Source.fromFile(path)) { data =>                              // load input data file
            val (rows, columns) = data.getLines.duplicate                   // duplicate data

            rows.flatMap(getVisibleInLine(_))                               // get row-wise visibility & make sequential
                      .zip(columns.toSeq                                    // zip with column-wise visibility
                                  .transpose                                // columns acquired by transposing the data
                                  .map(getVisibleInLine)                    // get column-wise visibility
                                  .transpose                                // transpose back so trees line up
                                  .flatten)                                 // make sequential for zipping with rows
                      .count(visibility => visibility._1 || visibility._2)  // count trees that are visible
        }.get                                                               // get final result
    }

    /*
    Day 8 part 1 not yet implemented. Finding a way to do this efficiently has been very challenging, so it's on hold.
     */


    /**
     * Day 9 part 1: Find the number of locations visited by the tail of the rope
     *
     * @param path path to input file
     * @return number locations visited
     */
    def day9part1(path: String): Int = {
        Using(Source.fromFile(path)) { data =>                                                  // load input data file
            val tailPositions = mutable.HashSet[(Int, Int)]()                                   // create set of visited positions

            data.getLines()                                                                     // for each line in file
                .foldLeft(((0,0), (0,0)))(                                                      // initialize initial head & tail positions to (0,0)
                    (prevLoc: ((Int, Int), (Int, Int)), line: String) => {                      // given the previous positions of the rope and the current instructions
                        var head = prevLoc._1                                                   // get the head position
                        var tail = prevLoc._2                                                   // get the tail position

                        (0 until line.drop(2).toInt).foreach(_ => {                             // for each step
                            line(0) match {                                                     // based on the direction of the instruction...
                                case 'L' => head = (head._1 - 1, head._2)                       // left: decrement X of head
                                case 'R' => head = (head._1 + 1, head._2)                       // right: increment X of head
                                case 'D' => head = (head._1, head._2 - 1)                       // down: decrement Y of head
                                case 'U' => head = (head._1, head._2 + 1)                       // up: increment Y of head
                            }
                            if ((head._1 - tail._1).abs > 1 || (head._2 - tail._2).abs > 1) {   // if the tail is too now far away from the head...
                                line(0) match {                                                 // based on the direction of the instruction...
                                    case 'L' => tail = (head._1 + 1, head._2)                   // left: tail is to the right of the head
                                    case 'R' => tail = (head._1 - 1, head._2)                   // right: tail is to the left of the head
                                    case 'D' => tail = (head._1, head._2 + 1)                   // down: tail is above the head
                                    case 'U' => tail = (head._1, head._2 - 1)                   // up: tail is below the head
                                }
                                tailPositions.add(tail)                                         // add the tail position to set of visited positions
                            }
                        })

                        (head, tail)                                                            // pass forward the rope's new positions
                    }
                )

            tailPositions.size                                                                  // compute the number of unique positions visited
        }.get                                                                                   // get final result
    }


    /**
     * Day 9 part 2: Find the number of locations visited by the tail of a rope with multiple knots
     * Note: Problem originally requested 10 knots. Abstracted to to an arbitrary number so this solution can also handle part 1!
     *
     * @param path path to input file
     * @param knots number of knots in the rope
     * @return number locations visited
     */
    def day9part2(path: String, knots: Int): Int = {
        Using(Source.fromFile(path)) { data =>                                                                                  // load input data file
            val tailPositions = mutable.HashSet[(Int, Int)]()                                                                   // create set of visited positions

            data.getLines()                                                                                                     // for each line in file
                .foldLeft(Seq.fill(knots)((0, 0)))(                                                                             // initialize initial knot positions to (0,0)
                    (rope: Seq[(Int, Int)], line: String) => {                                                                  // given the previous position of the rope and the current instructions
                        (0 until line.drop(2).toInt).foldLeft(rope)(                                                            // initialized the initial rope position to the previous position
                            (prevRope: Seq[(Int, Int)], _) => {                                                                 // given the position of the rope following the previous step
                                val newRope = prevRope.scanLeft((Int.MinValue, Int.MinValue))(                                  // initialize the initial knot position to (Int.MinValue, Int.MinValue)
                                    (prevKnot: (Int, Int), curKnot: (Int, Int)) => {                                            // given the position of the previous knot and the position of the current knot
                                        if (prevKnot == (Int.MinValue, Int.MinValue)) {                                         // if the previous knot is the initial knot...
                                            line(0) match {                                                                     // based on the direction of the instruction...
                                                case 'L' => (curKnot._1 - 1, curKnot._2)                                        // left: decrement X of the current knot
                                                case 'R' => (curKnot._1 + 1, curKnot._2)                                        // right: increment X of the current knot
                                                case 'D' => (curKnot._1, curKnot._2 - 1)                                        // down: decrement Y of the current knot
                                                case 'U' => (curKnot._1, curKnot._2 + 1)                                        // up: increment Y of the current knot
                                            }
                                        } else if ((prevKnot._1 - curKnot._1).abs > 1 || (prevKnot._2 - curKnot._2).abs > 1) {  // otherwise, if the current knot is too far away from the previous knot...
                                            val xDiff = prevKnot._1 - curKnot._1                                                // get the difference in X
                                            val yDiff = prevKnot._2 - curKnot._2                                                // get the difference in Y
                                            (curKnot._1 + xDiff.sign, curKnot._2 + yDiff.sign)                                  // move the current knot one space in the direction of each difference (0 difference results in no movement in that direction)
                                        } else {                                                                                // otherwise...
                                            curKnot                                                                             // do not change the position of the current knot
                                        }
                                    }
                                )

                                tailPositions.add(newRope.last)                                                                 // add the tail position to set of visited positions
                                newRope.tail                                                                                    // pass forward the rope's new positions (tail to remove the extra (Int.MinValue, Int.MinValue) knot)
                            }
                        )

                    }
                )

            tailPositions.size                                                                                                  // compute the number of unique positions visited
        }.get                                                                                                                   // get final result
    }

    /*
    Note on day 9 solutions:
    Both solutions could be done without inserting into a HashSet by using scanLeft (instead of foldLeft) passing
    forward all of the rope's positions after each step (instead of just the final position) and then using
    .flatten.distinctBy(_.last).size. However this introduces a lot of unnecessary complexity for zero gain because
    .distinct and .distinctBy use HashSet under the hood.
     */


    /**
     * Day 10 part 1: Find the sum of signal strengths for cycles where the cycle count + 20 is a multiple of 40
     *
     * @param path path to input file
     * @return sum of signal strengths
     */
    def day10part1(path: String): Int = {
        Using(Source.fromFile(path)) { data =>                                          // load input data file
            data.getLines()                                                             // for each line in file
                .foldLeft((1, 1, 0))(                                                   // initialize starting CPU state to (1, 1, 0) (cycle number, register, sum of relevant signal strengths)
                    (prev: (Int, Int, Int), instruction: String) => {                   // given the previous CPU state and the current instruction
                        var cpu = (                                                     // update the CPU...
                          prev._1 + 1,                                                  // increment the cycle counter
                          prev._2,                                                      // register is unchanged
                          prev._3 + prev._1 * prev._2 * (((prev._1 + 20) % 40) == 0)    // add current current signal strength to running sum if cycle count + 20 is a multiple of 40
                        )
                        if (instruction.take(4) == "addx") {                            // if the instruction is to add to the register
                            cpu = (                                                     // update the CPU...
                              cpu._1 + 1,                                               // increment the cycle counter
                              cpu._2 + instruction.drop(5).toInt,                       // add provided value to the register
                              cpu._3 + cpu._1 * cpu._2 * (((cpu._1 + 20) % 40) == 0)    // add current current signal strength to running sum if cycle count + 20 is a multiple of 40
                            )
                        }
                        cpu                                                             // pass forward the new CPU state
                    }
                )._3                                                                    // get the sum of relevant signal strengths
        }.get                                                                           // get final result
    }


    /**
     * Day 10 part 2: Generate image output based on received signals
     *
     * @param path path to input file
     * @return string representing image output
     */
    def day10part2(path: String): String = {
        Using(Source.fromFile(path)) { data =>                                              // load input data file
            data.getLines()                                                                 // for each line in file
                .foldLeft((1, 1, ""))(                                                      // initialize starting CPU/CRT state to (1, 1, "") (cycle number, register, CRT output)
                    (prev: (Int, Int, String), instruction: String) => {                    // given the previous CPU/CRT state and the current instruction
                        var cpu = (                                                         // update the CPU/CRT...
                          prev._1 + 1,                                                      // increment the cycle counter
                          prev._2,                                                          // register is unchanged
                          prev._3.appended(                                                 // append to the CRT display...
                              if ((prev._2 until prev._2 + 3) contains prev._1 % 40) '#'    // lit pixel (#) if the register aligns with the sprite's position
                              else '.'                                                      // otherwise, unlit pixel (.)
                          )
                        )
                        if (instruction.take(4) == "addx") {                                // if the instruction is to add to the register
                            cpu = (                                                         // update the CPU/CRT...
                              cpu._1 + 1,                                                   // increment the cycle counter
                              cpu._2 + instruction.drop(5).toInt,                           // register is unchanged
                              cpu._3.appended(                                              // append to the CRT display...
                                  if ((cpu._2 until cpu._2 + 3) contains cpu._1 % 40) '#'   // lit pixel (#) if the register aligns with the sprite's position
                                  else '.'                                                  // otherwise, unlit pixel (.)
                              )
                            )
                        }
                        cpu                                                                 // pass forward the new CPU/CRT state
                    }
                )._3                                                                        // get the CRT output
                .grouped(40)                                                                // split output into 40-pixel wide rows
                .flatMap(_.concat("\n"))                                                    // add a new line character to the end of each row
                .mkString                                                                   // convert to string
        }.get                                                                               // get final result
    }



    def main(args: Array[String]): Unit = {
        println("Day 1 part 1: " + day1Part1("./in/day1.txt"))
        println("Day 1 part 2: " + day1Part2("./in/day1.txt", 3))
        println("Day 1 part 1 (using part 2 implementation): " + day1Part2("./in/day1.txt", 1))

        println("Day 2 part 1: " + day2part1("./in/day2.txt"))
        println("Day 2 part 2: " + day2part2("./in/day2.txt"))

        println("Day 3 part 1: " + day3part1("./in/day3.txt"))
        println("Day 3 part 2: " + day3part2("./in/day3.txt", 3))

        println("Day 4 part 1: " + day4part1("./in/day4.txt"))
        println("Day 4 part 2: " + day4part2("./in/day4.txt"))

        println("Day 5 part 1: " + day5("./in/day5.txt"))
        println("Day 5 part 2: " + day5("./in/day5.txt", part2 = true))

        println("Day 6 part 1: " + day6("./in/day6.txt", 4))
        println("Day 6 part 2: " + day6("./in/day6.txt", 14))

        println("Day 7 part 1: " + day7part1("./in/day7.txt"))
        println("Day 7 part 2: " + day7part2("./in/day7.txt"))

        println("Day 8 part 1: " + day8part1("./in/day8.txt"))

        println("Day 9 part 1: " + day9part1("./in/day9.txt"))
        println("Day 9 part 2: " + day9part2("./in/day9.txt", 10))
        println("Day 9 part 1 (using part 2 implementation):  " + day9part2("./in/day9.txt", 2))

        println("Day 10 part 1: " + day10part1("./in/day10.txt"))
        println("Day 10 part 2:\n" + day10part2("./in/day10.txt"))
    }
}