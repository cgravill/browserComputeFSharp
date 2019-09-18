using WebSharper;

namespace LibraryCS
{
	[JavaScript]
	public class Utility
    {
        public int Process(int value)
        {
            var random = new System.Random();
            var wait = random.Next(4);

            var finish = System.DateTime.Now.AddSeconds(wait);
            do {

            } while (System.DateTime.Now < finish);
            return value * 3;
        }
    }
}
