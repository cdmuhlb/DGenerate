import java.io.*;

public class ConcatYgraph {
  public static void main(final String[] args) throws IOException {
    if (args.length != 1) {
      System.err.println("Usage: ConcatYgraph <nElems>");
      System.exit(1);
    }
    final int nElems = Integer.parseInt(args[0]);

    BufferedReader[] readers = new BufferedReader[nElems];
    for (int elem=0; elem<nElems; ++elem) {
      readers[elem] = new BufferedReader(new FileReader(
          "element" + elem + ".yg"));
    }

    final PrintWriter out = new PrintWriter(System.out);
    lineLoop:
    while (true) {
      for (int elem=0; elem<nElems; ++elem) {
        // Print or skip header
        if (elem == 0) {
          final String header = readers[elem].readLine();
          if (header == null) break lineLoop;
          else {
            out.println(header);
          }
        } else {
          readers[elem].readLine();
        }

        // Copy data
        //for (int line=0; line<order+1; ++line) {
        for (String line=readers[elem].readLine(); !line.trim().isEmpty();
             line=readers[elem].readLine()) {
          //out.println(readers[elem].readLine());
          out.println(line);
        }

        // Print or skip footer (blank line)
        if (elem == nElems-1) {
          //out.println(readers[elem].readLine());
          out.println();
        } else {
          //readers[elem].readLine();
        }
      }
    }
    out.close();

    for (int elem=0; elem<nElems; ++elem) {
      readers[elem].close();
    }
  }
}
