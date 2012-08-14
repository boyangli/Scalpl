/**
 * 
 */
package jimpl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;


/**
 * @author sml3
 * see ghallab et al. pp 97 
 * and http://en.wikipedia.org/wiki/Transitive_closure
 * and Cormen, leiserson, rivest, and stein, section on Floyd-Warshall  
 * and McAllester D., and Rosenblitt D., AAAI-91
 * consider using a Fibonacci heap
 * note that a partial order on a set can be represented as a graph.
 * topological sort
 * cycle in transitive closure when a node can reach itself
 * 
 * On building the transitive reduction of a two-dimensional poset
 * http://www.sciencedirect.com/science?_ob=ArticleURL&_udi=B6V0F-3SP2BXF-G&_user=127053&_rdoc=1&_fmt=&_orig=search&_sort=d&_docanchor=&view=c&_searchStrId=1140694421&_rerunOrigin=google&_acct=C000010458&_version=1&_urlVersion=0&_userid=127053&md5=1f4875c256a98ace75c343e28b423494
 * 
 * Hasse (order) diagram
 * 
 * poset heap
 * 
 * http://people.cs.vt.edu/~yifeima/homepage/reading%20list/algorithms/algorithms%20introduced%20in%20MIT%20Alg.txt
 * http://www.cs.cornell.edu/Courses/cs312/2004sp/lectures/lec25.asp
 * http://www.cs.odu.edu/~toida/nerzic/content/relation/order/order.html
 * http://www.unc.edu/~rap/Posets/
 * 
 * http://en.wikipedia.org/wiki/Partially_ordered_set
 * 
 * The complexity of heaps
 * http://delivery.acm.org/10.1145/140000/139483/p393-carlsson.pdf?key1=139483&key2=9998511621&coll=GUIDE&dl=GUIDE&CFID=67296168&CFTOKEN=59500839
 */
public class OrderingFaster implements JOrdering
{
	//private static final org.apache.log4j.Logger log4j = org.apache.log4j.Logger.getLogger( OrderingFaster.class );
	
	//Key: stepId from the plan 
	//Value: internal id used by ordering constraint manager
	protected HashMap< Integer /*stepId*/, Integer /*orderID*/> hmStepIdToOrderId;
	
	//index is order id, value at index is the corresponding stepId for that orderID
	//the logical inverse of hmStepIdToOrderId, but using a smaller data structure
	protected ArrayList<Integer> alOrderIDtoStepID;
	
	//first access gets you the row, second the column.
	//the indexes are orderIDs (not the step id; see hmStepIdToOrderID and alOrderIDtoStepID)
	//eg if( edgeMatrix[ 2 ][ 1 ] == true ) then there is an edge FROM node 2 TO node 1 (representing 2<1) 
	protected boolean[][] edgeMatrix;

	/**
	 * Default constructor, makes an empty ordering constraint manager 
	 * with all fields initialized (not lazy instantiation).
	 */
	public OrderingFaster(  )
	{
		hmStepIdToOrderId = new HashMap<Integer, Integer>();
		alOrderIDtoStepID = new ArrayList<Integer>();
		edgeMatrix = new boolean[0][];
	}

	/**
	 * Copy constructor, makes deep copies of all internal fields for safety.
	 * 
	 * @param o the ordering constraint to copy
	 */
	public OrderingFaster( OrderingFaster o )
	{
		if( o == null )
		{
			hmStepIdToOrderId = new HashMap<Integer, Integer>( );
			alOrderIDtoStepID = new ArrayList<Integer>();
			edgeMatrix = new boolean[0][];
			return;
		}

		this.alOrderIDtoStepID = new ArrayList<Integer>( o.alOrderIDtoStepID );
		hmStepIdToOrderId = new HashMap<Integer, Integer>( o.hmStepIdToOrderId );
		
		this.edgeMatrix = new boolean[o.edgeMatrix.length][];
		for (int i = 0; i < o.edgeMatrix.length; i++) 
		{
			boolean[] reachList = o.edgeMatrix[i];
			boolean[] newList = new boolean[reachList.length];
			System.arraycopy(reachList, 0, newList, 0, reachList.length);
			this.edgeMatrix[i] = newList;
		}
	}
	
	public OrderingFaster copy() {
		return new OrderingFaster(this);
	}
	
	/**
	 * This is a query to see if a plan step is ordered to come before another: step1 < step2 
	 * It runs in constant time in this implementation.
	 * <p>
	 * Assumes that transitive closure is already maintained, and someone else is taking care of threats
	 * 
	 * @param idS1 the id that the first step calls itself (not the internal ordering id)
	 * @param idS2 the id that the second step calls itself (not the internal ordering id)
	 * @param accordingToTransitive unused in this implementation, because the transitive closure is always maintained
	 * @return true when s1 is ordered before s2, false otherwise
	 */
	@Override
	public final boolean orderedBefore( int idS1, int idS2)
	{
		//notice i manually check MIN and MAX here because i assume these are not kept in the trix
		//i should probably verify that this is in practice the faster method. 
		//otherwise, i should let ordering handle min in max instead of using auxilary checks. 
		//Intuition, however, tells me this is both a smaller and faster implementation.  
		if( idS2 == JOrdering.ID_MIN || idS1 == JOrdering.ID_MAX )
			return false;
		if( idS1 == JOrdering.ID_MIN || idS2 == JOrdering.ID_MAX )
			return true;

		Integer lhsOrderId = hmStepIdToOrderId.get( idS1 );
		Integer rhsOrderId = hmStepIdToOrderId.get( idS2 );

		if( lhsOrderId == null || rhsOrderId == null || lhsOrderId == rhsOrderId )
			return false;

		return edgeMatrix[lhsOrderId][rhsOrderId]; //it is before if orderID1 < orderID2
	}
	

	/**
	 * This is NOT a s1 < s2 query (that is, this is not the method isOrderedBefore).
	 * Instead it asks if s1 < s2 can be added to the existing ordering constraints 
	 * without creating any loops/cycles (which are bad, and why this method exists).
	 * 
	 * Note that asking an ordering about possibility assumes someone else is taking care of threats. 
	 * This is a strict answer to logical queries of ordering relations.
	 * 
	 * @param idS1 the id that the first step calls itself (not the internal ordering id)
	 * @param idS2 the id that the second step calls itself (not the internal ordering id)
	 * @param accordingToTransitive unused in this implementation, because the transitive closure is always maintained
	 * @return true when s1 can be consistently (acyclically) ordered before s2, false otherwise
	 */
	@Override
	public final boolean possiblyBefore( int idS1, int idS2)
	{//notice i manually check MIN and MAX here because i assume these are not kept in the trix
		//i should probably verify that this is in practice the faster method. otherwise, i should let ordering handle min in max instead of using auxilary checks. Intuition, however, tells me this is both a smaller and faster implementation.
//		if( DCPOP.LOG4J_ON && log4j.isDebugEnabled() )
//			log4j.debug( "OrderingFaster.possiblyBefore( s("+idS1+"), ("+idS2+"), " + " accordingToTransitive= " + accordingToTransitive +" )"  );
		if( idS1 == idS2 ) //a step can't come before itself
		{
//			if( DCPOP.LOG4J_ON && log4j.isDebugEnabled() )
//				log4j.debug( "    OrderingFaster.possiblyBefore(): returning false, because a step cannot come before itself." );
			return false;
		}
		if( idS2 == JOrdering.ID_MIN || idS1 == JOrdering.ID_MAX )
		{
//			if( DCPOP.LOG4J_ON && log4j.isDebugEnabled() )
//				log4j.debug( "    OrderingFaster.possiblyBefore(): returning false, because lhs==MaxID, or rhs==MinID" );
			return false;
		}
		if( idS1 == JOrdering.ID_MIN || idS2 == JOrdering.ID_MAX )
		{
//			if( DCPOP.LOG4J_ON && log4j.isDebugEnabled() )
//				log4j.debug( "    OrderingFaster.possiblyBefore(): returning true, because lhs==MinID, or rhs==MaxId" );
			return true;
		}

		Integer lhsOrderId = hmStepIdToOrderId.get( idS1 );
		Integer rhsOrderId = hmStepIdToOrderId.get( idS2 );
	
		if( lhsOrderId == null || rhsOrderId == null )
		{
//			if( DCPOP.LOG4J_ON && log4j.isDebugEnabled() )
//			{
//				log4j.debug( "    OrderingFaster.possiblyBefore(): One or both of the steps being checked is not in the adjacency trix, and i'm returning true (maybe should be returning false?). lhs,rhs->"+idS1+","+idS2 );
//			}
			return true;
		}

		return possiblyBeforePrivate( lhsOrderId, rhsOrderId);
	}
	
	//expects orderID1 and orderID2 to be valid references (possiblyBefore with Step params checks for this)
	//also note that it's not checking that orderID1 < orderID2 according to the matrix
	//like above, it's asking if the constraint can be added to the trix without causing a loop (bad) 
	private final boolean possiblyBeforePrivate( int orderID1, int orderID2)
	{//i do NOT check for step min/max here, because i assume the overloaded meth. above with Steps checks this.
		if( orderID1 == orderID2 )
			return false;
		
		//it is possible before if orderID1 < orderID2, or if orderID2 !< orderID1
		return edgeMatrix[orderID1][orderID2] || !edgeMatrix[orderID2][orderID1];
	}

	/**
	 * Adds the ordering constraint earlierStepID < laterStepID, and maintains transitive closure.
	 * This method does not ensure the graph remains acyclic. Think of this method as ADD_EDGE with closure. 
	 * It does add dictionary mappings and expands the data structures if needed.
	 * 
	 * @param earlierStepID the id that the first step calls itself (not the internal ordering id)
	 * @param laterStepID the id that the second step calls itself (not the internal ordering id)
	 * @param doTransitiveClosure unused in this implementation, because the transitive closure is always maintained
	 */
	@Override
	public final void addOrder( int earlierStepID, int laterStepID)
	{	
//		if( DCPOP.LOG4J_ON && log4j.isDebugEnabled() )
//			log4j.debug( "OrderingFaster.addOrderWithClosure("+earlierStepID+","+laterStepID+", doTransitiveClosure="+doTransitiveClosure+" )" );

		//i manually check for min and max here because i assume we don't store these
		if( earlierStepID == JOrdering.ID_MIN || laterStepID == JOrdering.ID_MAX )
		{//we're not keeping closure for min and max because we can assume this.
			return;
		}
		boolean hasLHS = hmStepIdToOrderId.containsKey( earlierStepID );
		boolean hasRHS = hmStepIdToOrderId.containsKey( laterStepID );
		
		if( !hasLHS && !hasRHS )
		{//if they're both new, needn't do anything for closure
//			if( DCPOP.LOG4J_ON && log4j.isDebugEnabled() )
//			{
//				lNewRNew++;
//				log4j.debug( "Both steps new in ordering..." );
//			}

			int iOrderId_LHS = nextAvailableOrderID();
			hmStepIdToOrderId.put( earlierStepID, iOrderId_LHS );
			alOrderIDtoStepID.add( earlierStepID );
			
			int iOrderId_RHS = nextAvailableOrderID();
			hmStepIdToOrderId.put( laterStepID, iOrderId_RHS );
			alOrderIDtoStepID.add( laterStepID );

			//expand all rows by two
			expandEdgeTrixByTwo();
			
			edgeMatrix[iOrderId_LHS][iOrderId_RHS]= true;
			
		}
		else if( hasLHS && !hasRHS )
		{//LHS exists, RHS new; everything that is before lhs is also before rhs. rhs is before nothing but goal step
//			if( DCPOP.LOG4J_ON && log4j.isDebugEnabled() )
//			{
//				log4j.debug( "LHS exists, RHS is new..." );
//				rNew++;
//			}

			int iOrderId_RHS = nextAvailableOrderID();
			hmStepIdToOrderId.put( laterStepID, iOrderId_RHS );
			alOrderIDtoStepID.add( laterStepID );
                        
			int iOrderId_LHS = hmStepIdToOrderId.get( earlierStepID );

			//expand all rows by one
			expandEdgeTrixByOne();

			for (int i = 0; i < edgeMatrix.length - 1; i++) 
			{
				if (edgeMatrix[i][iOrderId_LHS]) // this step says it's before LHS, so make it also before RHS
					edgeMatrix[i][iOrderId_RHS] = true;
				// else //this step is not before LHS, so it is not before RHS
				// (leave default false value)
			}

			edgeMatrix[ iOrderId_LHS ][ iOrderId_RHS ] = true ;			
		}
		else if( !hasLHS && hasRHS )
		{//LHS new, RHS exists; lhs is before everything that rhs is before (what is lhs after?)
//			if( DCPOP.LOG4J_ON && log4j.isDebugEnabled() )
//			{
//				lNew++;
//				log4j.debug( "LHS is new, RHS exists..." );
//			}

			int iOrderId_LHS = nextAvailableOrderID();
			hmStepIdToOrderId.put( earlierStepID, iOrderId_LHS );
			alOrderIDtoStepID.add( earlierStepID );

			int iOrderId_RHS = hmStepIdToOrderId.get( laterStepID );
                        
			//expand all rows by one
			expandEdgeTrixByOne();

			//add new row that is copy of adjTrix.getReachs( RHS ), as well as 	setReaches( LHS, RHS, true )
			System.arraycopy(edgeMatrix[iOrderId_RHS], 0, edgeMatrix[iOrderId_LHS], 0, edgeMatrix[iOrderId_RHS].length);
			edgeMatrix[iOrderId_LHS][iOrderId_RHS] = true;
		}
		else if( hasLHS && hasRHS )
		{//LHS exists, RHS exists. Oh boy, this is a tricky one! Must ensure closure.
		    /*
		     * All steps ordered before LHS (and LHS itself) must be ordered
		     * before RHS and all steps ordered after RHS.
		     */                     

//			if( DCPOP.LOG4J_ON && log4j.isDebugEnabled() )
//			{
//				noNew++;
//				log4j.debug( "LHS exists, RHS exists..." );
//			}
			//setReaches( LHS, RHS, true )
			int iOrderId_LHS = hmStepIdToOrderId.get( earlierStepID );
			int iOrderId_RHS = hmStepIdToOrderId.get( laterStepID );

            if( ! edgeMatrix[iOrderId_LHS][iOrderId_RHS] )
            {//only do the work if we have to. ie, lhs reaches rhs was false, and we're changing it to true
				int n = alOrderIDtoStepID.size();
				for (int k = 0; k < n; k++) 
				{
					if ((k == iOrderId_LHS || edgeMatrix[k][iOrderId_LHS])
							&& !edgeMatrix[k][iOrderId_RHS]) 
					{
						for (int l = 0; l < n; l++) 
						{
							if ((iOrderId_RHS == l || edgeMatrix[iOrderId_RHS][l])
									&& !edgeMatrix[k][l]) 
							{
								if (k != l)
									edgeMatrix[k][l] = true;
							}
						}
					}
				}
            }
		}
	}	
	
	// assumes the parentid exists; if it doesn't we assume the parent was only ordered between the initial and goal steps
	//call this BEFORE adding the orderings of the children steps to one-another
	//child may or may not be already in plan.
	//assumes the caller has checked the possibility/sanity of adding parent's constraints to any child that already exists
	public void inheritOrdering(int parentStepID, int[] existing, int[] inserted) 
	{
		if( !hmStepIdToOrderId.containsKey( parentStepID ) )
		{//the parent is not in the ordering constraints; we assume this means it was only ordered between the init and goal steps
			//TODO we can make this more efficient by adding the steps we know will be added in a moment
			return; //for now, do an expensive growth			
		}
		
		int iParentOrderingID = hmStepIdToOrderId.get( parentStepID );
		
		//make new mappings for the inserted steps
		for( int childStepID : inserted )
		{
			if( hmStepIdToOrderId.containsKey( childStepID ) )
			{//the child already exists
				System.err.println( "LIAR! Told me this was a new step, but I don't buy it." );
			}
			//the child is a new step, make a new ord id for it, and update the mappings
			int iNewOrderId = nextAvailableOrderID();
			hmStepIdToOrderId.put( childStepID, iNewOrderId );
			alOrderIDtoStepID.add( childStepID );
		}
		
		//collect the ids of the existing steps
		ArrayList<Integer> reusedStepsOrdIds = new ArrayList<Integer>( inserted.length );
		for( int childStepID : existing )
		{
			if( hmStepIdToOrderId.containsKey( childStepID ) )
			{//the child already exists
				reusedStepsOrdIds.add( hmStepIdToOrderId.get( childStepID ) );
			}
			else
			{
				int iNewOrderId = nextAvailableOrderID();
				hmStepIdToOrderId.put( childStepID, iNewOrderId );
				alOrderIDtoStepID.add( childStepID );
				reusedStepsOrdIds.add( iNewOrderId );
				
				//expand all rows by one
				expandEdgeTrixByOne();
			}
		}
		
		//expand all rows by N, where N is the number of new steps being added, and inherit the ordering of the parent to the newsteps
		expandEdgeTrixByN( inserted.length, iParentOrderingID );
		
		//add the parent's ordering to the reused steps
		boolean[] parentsRow = edgeMatrix[ iParentOrderingID ];
		for( int i=0; i<parentsRow.length; i++ )
		{
			if( parentsRow[i] == true )
			{
				for( int reusedChildStepID : reusedStepsOrdIds )
				{
					edgeMatrix[reusedChildStepID][i]= true;
				}
			}
		}
		
	}
	
	private final int nextAvailableOrderID()
	{
		return alOrderIDtoStepID.size();
	}	
       
    private final void expandEdgeTrixByOne()
    {
        boolean[][] edgeMatrixCopy = new boolean[ edgeMatrix.length+1 ][];
        for( int i=0; i<edgeMatrix.length; i++)
        {
            boolean[] newReaches = new boolean[ edgeMatrixCopy.length ];
            System.arraycopy(edgeMatrix[i], 0, newReaches, 0, edgeMatrix[i].length);
            edgeMatrixCopy[i] = newReaches;
        }
        edgeMatrixCopy[ edgeMatrixCopy.length-1 ] = new boolean[ edgeMatrixCopy.length ];
        edgeMatrix = edgeMatrixCopy;
    }
        
    private final void expandEdgeTrixByTwo()
    {
        boolean[][] edgeMatrixCopy = new boolean[ edgeMatrix.length+2 ][];
        for( int i=0; i<edgeMatrix.length; i++)
        {
            boolean[] newReaches = new boolean[ edgeMatrixCopy.length ];
            System.arraycopy(edgeMatrix[i], 0, newReaches, 0, edgeMatrix[i].length);
            edgeMatrixCopy[i] = newReaches;
        }
        edgeMatrixCopy[ edgeMatrixCopy.length-2 ] = new boolean[ edgeMatrixCopy.length ];
        edgeMatrixCopy[ edgeMatrixCopy.length-1 ] = new boolean[ edgeMatrixCopy.length ];
        edgeMatrix = edgeMatrixCopy;
    }
    
    //N > 0
    //each child should start with the parent's reachability row
  	//then, for each step that is ordered before parent, update the child row to have same constraint
    private final void expandEdgeTrixByN( int numNewSteps, int parentOrdID  )
    {
        boolean[][] edgeMatrixCopy = new boolean[ edgeMatrix.length+numNewSteps ][];
        for( int i=0; i<edgeMatrix.length; i++)
        {//expand each row by N
            boolean[] newReaches = new boolean[ edgeMatrixCopy.length ];
            edgeMatrixCopy[i] = newReaches;
            System.arraycopy(edgeMatrix[i], 0, newReaches, 0, edgeMatrix[i].length);

            if( edgeMatrix[i][parentOrdID] == true )
            {//current step was ordered to come before parent, so it will also come before parent's children
            	Arrays.fill(newReaches, edgeMatrix.length, newReaches.length, true);
            }//else leave the reachability false
 
        }
        boolean[] newRow = new boolean[ edgeMatrixCopy.length ];
    	System.arraycopy(edgeMatrix[parentOrdID], 0, newRow, 0, edgeMatrix[parentOrdID].length);
        for( int i=numNewSteps; i > 0; i-- ){
        	edgeMatrixCopy[ edgeMatrixCopy.length-i ] = newRow.clone();
//        	System.arraycopy(edgeMatrix[parentOrdID], 0, edgeMatrixCopy[ edgeMatrixCopy.length-i ], 0, edgeMatrix[parentOrdID].length);
        }
        edgeMatrix = edgeMatrixCopy;
    }
	
    private static int lNewRNew = 0, rNew=0, lNew=0, noNew=0;    
    public static void printOrderStats()
    {
        System.out.println( "total times addORderWithClosureCalled: " + (lNewRNew+rNew+lNew+noNew) );
        System.out.println( "both steps new: " + lNewRNew );
        System.out.println( "l exists, r new: " + rNew );
        System.out.println( "l new, r exists: " + lNew );
        System.out.println( "both exists: " + noNew );
    }	
	
	//checks if an edge matrix rep of a graph has a cycle (ie a node that can reach itself)
	//transitive closure SHOULD be computed first.
    @Deprecated
	public static boolean isAcyclic( ArrayList<ArrayList<Boolean>> trix )
	{
		for( int i=0; i<trix.size(); i++ )
		{
			if( trix.get( i ).get( i ) == true )
			{
				return false;
			}
		}
		return true;
	}
	
  //minus 1 because the initial step (s0) is in p.steps; the goal step is not
    /**
     * Makes one valid linearization of the ordering constraints. 
     * Note that there may be many valid linearizations, however this will always return the same one.
     *  
     * @param allStepIds all step ids (not ordering ids) of the plan for which this ordering manager exists, exclusive of s0 and sInf
     * @return a linearization of the ordering constraints that is consistent
     */
    @Override
	public final int[] topsort( int[] allStepIds )
	{
		int[] totallyOrderedStepIDs = new int[ allStepIds.length ]; 
		
		List<Integer> orderedOrderIDs = OrderingFaster.doTopologicalSort_brayray( edgeMatrix );

		for( int i=0; i<orderedOrderIDs.size(); i++ )
		{//lookup all steps that we have constraints on, and add to the ordered array
			totallyOrderedStepIDs[i] =  alOrderIDtoStepID.get( orderedOrderIDs.get( i ) ); 
		}
		if( orderedOrderIDs.size() < totallyOrderedStepIDs.length )
		{//now add the unconstrained steps
			int openSpot = orderedOrderIDs.size();
			for( int sID : allStepIds )
			{
				if( sID != JOrdering.ID_MIN && sID != JOrdering.ID_MAX )
				{
					if( !hmStepIdToOrderId.containsKey( sID ) )
					{//ahhah. a step that is unconstrained
						totallyOrderedStepIDs[openSpot++] = sID;
					}
				}
			}
		}
		return totallyOrderedStepIDs;
	}	
	
	private static final LinkedList<Integer> doTopologicalSort_brayray( boolean[][] _transClosure )
	{
//		if( DCPOP.LOG4J_ON && log4j.isDebugEnabled() )
//			log4j.debug( "doTopologicalSort_brayray" );
		LinkedList<Integer> ansList = new LinkedList<Integer>();

		if( _transClosure == null )
		{
			return ansList;
		}
		if( _transClosure.length == 0 )
		{
			return ansList;
		}
		
		int numNodes = _transClosure.length;
		
//		if( DCPOP.LOG4J_ON && log4j.isDebugEnabled() )
//		{
//			StringBuilder sb = new StringBuilder();
//			for( int i=0; i<numNodes; i++ )
//			{
//				for( int j=0; j<_transClosure[i].length; j++ )
//				{
//					sb.append( " " + (_transClosure[i][j] == true ? "1" : "0") );
//				}
//				log4j.debug( sb );
//			}
//		}
		
		int time = 0;
		boolean[] visited = new boolean[ numNodes ];
		int[] discovered = new int[ numNodes ];
		int[] finished = new int[ numNodes ];
		Arrays.fill( visited, false );
		
		//for each vertex...
		for( int i=0; i<numNodes; i++ )
		{
			//if we haven't visited this vertex yet... (ie the vertex is white)
			if( !visited[i] )
			{
				//explore recursively each child
				time = DFSvisit_TopologicalSort_brayray( _transClosure, i, time, visited, discovered, finished, ansList );				
			}
		}
		return ansList;		
	}
	
	private static final int DFSvisit_TopologicalSort_brayray( boolean[][] _transClosure, int indexParent, int time, boolean[] visited, int[] discovered, int[] finished, LinkedList<Integer> ansList )
	{
		//mark this node visited, note the time, and recurse on children
		visited[ indexParent ] = true; 
		discovered[ indexParent ] = ++time; //ie gray
		
		//for each adjacent (child) node of index....
		boolean[] children = _transClosure[ indexParent ];
		for( int iCurChild = 0; iCurChild < children.length; iCurChild++ )
		{//TODO we may want to randomize the children to get different linearizations of the plan
			//if this node is a child and we haven't visited this node yet...
			if( children[ iCurChild ] == true  )
			{				
				if( !visited[iCurChild] )
				{
					time = DFSvisit_TopologicalSort_brayray( _transClosure, iCurChild, time, visited, discovered, finished, ansList );
				}
				else if( discovered[ iCurChild ] > 0 && finished[ iCurChild ] == 0 )
				{
//					log4j.fatal( "DFSvisit_TopologicalSort_brayray ERROR: Cycle detected!!!" );
					System.exit( 1 );
				}
			}
		}
		finished[indexParent] = ++time;//ie black
		ansList.addFirst( indexParent );
		return time;
	}
	
	/**
	 * For debugging purposes.
	 */
	public void printEdgeMatrix()
	{//TODO should this depend upon log level?
		for( int i=0; i<alOrderIDtoStepID.size(); i++ )
		{
			System.out.printf( "O(%d) = S(%d)%n", i, alOrderIDtoStepID.get( i ) );
		}
		OrderingFaster.print( edgeMatrix );
	}
       
	private static void print( boolean[][] trix )
	{
		int size = trix.length;
		for( int i=0; i<size; i++ )
		{
			for( int j=0; j<size; j++ )
			{
				System.out.print( trix[ i ][ j ] == true ? "1" : "0" );
			}
			System.out.println();
		}
	}

	public StringBuilder toStr()
	{
		StringBuilder sb = new StringBuilder();
		for( int i=0; i<alOrderIDtoStepID.size(); i++ )
		{
			int lhsStepID = alOrderIDtoStepID.get( i );
			for( int j=0; j<alOrderIDtoStepID.size(); j++ )
			{
				if( edgeMatrix[ i ][ j ] )
				{
					sb.append( "S(" + alOrderIDtoStepID.get( i ) + ") < " + "S(" + alOrderIDtoStepID.get( j ) + "), " );
				}
				
			}
		}
		
		if( sb.length() > 0 )
		{//removes final ", "
			sb = sb.deleteCharAt( sb.length() - 1 );
			sb = sb.deleteCharAt( sb.length() - 1 );
		}
		return sb;
	}

	/**
	 * In this implementation, this method doesn't have to do anything (because it maintains closure always).
	 */
	@Override
	public void computeTransitiveClosure() {
		
	}
	
	//public int[][] necessary() 

}

//google Counting Linear Extensions

//http://mathworld.wolfram.com/LinearExtension.html
//Brightwell, G. and Winkler, P. "Counting Linear Extensions." Order 8, 225-242, 1991.
//  http://www.springerlink.com/content/p395864591l07770/fulltext.pdf
//Brualdi, R. A. Introductory Combinatorics, 4th ed. New York: Elsevier, 1997.
//Bubley, R. and Dyer, M. "Faster Random Generation of Linear Extensions." In Proc. Ninth Annual ACM-SIAM Symposium on Discrete Algorithms, San Francisco, Calif., pp. 350-354, 1998.
//  http://delivery.acm.org/10.1145/320000/314730/p350-bubley.pdf?key1=314730&key2=4889892621&coll=GUIDE&dl=GUIDE&CFID=70261782&CFTOKEN=29962468
//Preusse, G. and Ruskey, F. "Generating Linear Extensions Fast." SIAM J. Comput. 23, 373-386, 1994.
//Varol, Y. and Rotem, D. "An Algorithm to Generate All Topological Sorting Arrangements." Comput. J. 24, 83-84, 1981. 

//see also http://eprints.kfupm.edu.sa/72293/1/72293.pdf

//RAO says
//"It can be shown [8] that a partial plan is consistent if it has at least one 
//safe ground linearization, and inconsistent otherwise."
//http://rakaposhi.eas.asu.edu/tradeoffs-aips94.pdf
class FastCountLinearizations
{
	//wiki says the num of total-order linearizations for a given POP is
	//numLin = |actions|! / ( 2^(|orderingConstraints|) )
	//this is clearly wrong. do a couple simple examples for yourself.
}


//An algorithm to generate all topological sorting arrangements
//YL Varol, D Rotem - The Computer Journal, 1981 - Br Computer Soc
//http://comjnl.oxfordjournals.org/cgi/reprint/24/1/83
class AllTopologicalSorts
{
	public static void topsort( boolean[][] transClosurePoset )
	{
		int numElements = transClosurePoset.length;
		
	}
}

//Generating linear extensions fast
//G Pruesse, F Ruskey - SIAM Journal on Computing, 1994
//http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.52.3057&rep=rep1&type=pdf
class FastLinearExtensions
{
	public static void preprocess( boolean[][] transClosurePoset )
	{
		int i=0, j=0;
//		boolean[][] Q = new boolean[transClosurePoset.length][];
		int[][] QadjList = new int[ transClosurePoset.length ][];
		
		ArrayList<Integer> minElements = new ArrayList<Integer>();
		
		boolean[] isMinRay = new boolean[ transClosurePoset.length ];
		Arrays.fill( isMinRay, true );
		
		ArrayList<Integer> tempIntRay = new ArrayList<Integer>( transClosurePoset.length );
		
		for( int tmpI=0; tmpI<transClosurePoset.length; tmpI++ )
		{
//			Q[tmpI] = Arrays.copyOf( transClosurePoset[tmpI], transClosurePoset[tmpI].length );
			
			for( int tmpJ=0; tmpJ<transClosurePoset.length; tmpJ++ )
			{
				if( transClosurePoset[tmpI][tmpJ] == true )
				{
					tempIntRay.add( tmpJ );
					isMinRay[tmpJ] = false;
				}
			}
			
			QadjList[tmpI] = new int[tempIntRay.size()];
			for( int tmpJ=0; tmpJ<tempIntRay.size(); tmpJ++ )
			{
				QadjList[tmpI][tmpJ] = tempIntRay.get( tmpJ );
			}
			tempIntRay.clear();				
		}
		
		for( int tmpI=0; tmpI<isMinRay.length; tmpI++ )
		{
			if( isMinRay[tmpI] == true )
			{
				minElements.add( tmpI );
			}
		}
		
	}
}

