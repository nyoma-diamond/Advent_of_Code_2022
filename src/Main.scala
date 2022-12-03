import scala.io.Source
import scala.util.Using


object Main {

    /**
     * Day 1 part 1: Find the maximum total calories among the elves
     *
     * @param path path to input file
     * @return maximum total calories
     */
    def day1Part1(path: String): Int = {
        Using(Source.fromFile(path)) { data =>                          // load input data file
            data.getLines()                                             // for each line in file
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
     * @param n number of highest values to get
     * @return sum of highest `n` total calories
     */
    def day1Part2(path: String, n: Int): Int = {
        Using(Source.fromFile(path)) { data =>                                                      // load input data file
            (data.getLines() ++ Seq(""))                                                            // for each line in file (adds an extra empty line so the maximums get updated for the final sum)
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
            data.getLines()                                 // for each line in file
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
            data.getLines()                                 // for each line in file
                .foldLeft(0)(                               // initialize partial score to 0
                    (score: Int, cur: String) =>            // given partial score and current line
                        score +                             // add to partial score
                          (cur(2) - 'X') * 3 +              // add points for win/draw/loss
                          (cur(2) + cur(0) - 1) % 3 + 1     // add points for hand choice (wacky math Just Works)
                )
        }.get                                               // get final result
    }


    /**
     * Get the priority for an item
     *
     * @param item item to get priority of
     * @return int representing the priority
     */
    def itemPriority(item: Char): Int = {
        item % ('a' - 'A') + (              // offset so 'a' and 'A' start at 1
          if (item.isUpper)                 // if character is upercase
              'z' - 'a' + 1                 // offset priority up by 26
          else                              // otherwise
              0                             // no offset
          )
    }


    /**
     * Day 3 part 1: Find the sum of priorities of items in two compartments of the same rucksack
     *
     * @param path path to input file
     * @return sum of priorities
     */
    def day3part1(path: String): Int = {
        Using(Source.fromFile(path)) { data =>                          // load input data file
            data.getLines()                                             // for each line in file
                .foldLeft(0)(                                           // initialize partial sum to 0
                    (sum: Int, rucksack: String) => {                   // given partial sum and current rucksack (line)
                        sum + itemPriority((                            // add priority of shared item (found below) to partial sum
                          rucksack.take(rucksack.length / 2)            // use the left compartment to...
                            intersect                                   // compute the intersection...
                            rucksack.takeRight(rucksack.length / 2)     // with the right compartment
                        )(0))                                           // get the item
                    }
                )
        }.get                                                           // get final result
    }


    /**
     * Day 3 part 2: Find the sum of priorities of badges
     * Note: Problem originally stated that badges are shared by 3 adjacent elves. Abstracted to `n` because I can
     *
     * @param path path to input file
     * @param n number of adjacent elves to find badges in
     * @return sum of priorities of badges
     */
    def day3part2(path: String, n: Int): Int = {
        Using(Source.fromFile(path)) { data =>                              // load input data file
            data.getLines()                                                 // for each line in file
                .sliding(n, n)                                              // split rucksacks (lines) by desired group size
                .foldLeft(0)(                                               // initialize partial sum to 0
                    (sum: Int, sacks: Seq[String]) => {                     // given partial sum and current group
                        sum + itemPriority(                                 // add priority of badge (found below) to partial sum
                            sacks.foldLeft(('A' to 'z').mkString)(    // within the group, initialize partial list of shared items
                                (shared: String, rucksack: String) =>       // given the partial list of shared items and a rucksack
                                    shared intersect rucksack               // compute the intersection of partial shared items and the rucksack
                            )(0)                                            // get the badge
                        )
                    }
                )
        }.get                                                               // get final result
    }


    def main(args: Array[String]): Unit = {
        println("Day 1 part 1: " + day1Part1("./in/day1.txt"))
        println("Day 1 part 2: " + day1Part2("./in/day1.txt", 3))
        println("Day 1 part 1 (using part 2 implementation): " + day1Part2("./in/day1.txt", 1))

        println("Day 2 part 1: " + day2part1("./in/day2.txt"))
        println("Day 2 part 2: " + day2part2("./in/day2.txt"))

        println("Day 3 part 1: " + day3part1("./in/day3.txt"))
        println("Day 3 part 2: " + day3part2("./in/day3.txt", 3))
    }
}