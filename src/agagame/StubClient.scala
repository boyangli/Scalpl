package agagame

import java.io._
import java.net._

class StubClient {
  var requestSocket: Socket = null
  var out: ObjectOutputStream = null
  var in: ObjectInputStream = null
  var message: String = "";

  def run() {
    
    
    
    try {
      //1. creating a socket to connect to the server
      requestSocket = new Socket("localhost", 2013);
      System.out.println("Connected to localhost in port 2013");
      //2. get Input and Output streams
      out = new ObjectOutputStream(requestSocket.getOutputStream());
      out.flush();
      in = new ObjectInputStream(requestSocket.getInputStream());
      //3: Communicating with the server
      do {
        try {
          message = in.readObject().asInstanceOf[String];
          System.out.println("server>" + message);
          sendMessage("0101111");
          message = in.readObject().asInstanceOf[String];
          System.out.println("server>" + message);
          message = "bye";
          sendMessage(message);
        } catch {
          case err: ClassNotFoundException =>
            System.err.println("Data received in unknown format");
        }
      } while (!message.equals("bye"));
    } catch {
      case unknownHost: UnknownHostException =>
        System.err.println("You are trying to connect to an unknown host!");
      case ioException: IOException =>
        ioException.printStackTrace();
    } finally {
      //4: Closing connection
      try {
        in.close();
        out.close();
        requestSocket.close();
      } catch {
        case ioException: IOException =>
          ioException.printStackTrace();
      }
    }
  }

  def sendMessage(msg: String) {
    try {
      out.writeObject(msg);
      out.flush();
      System.out.println("client>" + msg);
    } catch {
      case ioException: IOException =>
        ioException.printStackTrace();
    }
  }
}

object StubClient {
  def main(args: Array[String]) {
    val client = new StubClient();
    client.run();
  }
}