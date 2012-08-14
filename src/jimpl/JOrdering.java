package jimpl;

import java.util.List;
import planning.Constants;
public interface JOrdering
{
	public boolean orderedBefore( int idS1, int idS2);
	public void addOrder( int earlierStepID, int laterStepID);
	public StringBuilder toStr();
	public boolean possiblyBefore( int idS1, int idS2);
	public void computeTransitiveClosure();
	public int[] topsort( int[] allStepIds );
	public void inheritOrdering(int parent, int[] existing, int[] inserted);
	 
	public JOrdering copy();
	public static final int ID_MIN = 0; //the lowest id a step can have (ie S0)
	public static final int ID_MAX = Constants.GOAL_ID(); //the highest id a step can have (ie Sinf)
	
}
