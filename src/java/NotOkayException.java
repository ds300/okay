package okay;

/**
 * This exception gets thrown when something is just not ok.
 */
public class NotOkayException extends Exception {
  public final Object keyPath;
  public final Object badValue;
  public final Object originalException;

  public NotOkayException (String msg, Object keyPath, Object badValue, Exception originalException) {
    super(msg);
    this.keyPath = keyPath;
    this.badValue = badValue;
    this.originalException = originalException;
  }

  @Override
  public String toString() {
    String s = getMessage();
    if (keyPath != null) {
      s += "\nBad value for field " + keyPath.toString() + ": " + badValue;
    }
    if (originalException != null) {
      s += "\nOriginal Exception:\n" + originalException.toString();
    }
    return s;
  }
}