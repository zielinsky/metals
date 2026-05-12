package definition

import coursierapi.Dependency
import tests.pc.BaseJavaTypeDefinitionSuite

class TypeDefinitionSuite extends BaseJavaTypeDefinitionSuite {

  override protected def extraDependencies: Seq[Dependency] = List(
    Dependency.of("com.google.guava", "guava", "31.1-jre")
  )

  check(
    "within-file",
    """
      |import java.util.Arrays;
      |import java.util.List;
      |class A {
      |    public static A create() { return new A(); }j
      |    public static A foo = cre@@ate()
      |}
      |""".stripMargin,
    """|Definition.java:5:7: info: definition
       |class A {
       |      ^
       |""".stripMargin,
  )

  check(
    "within-file-to-external",
    """
      |import java.util.Arrays;
      |import java.util.List;
      |class A {
      |    public static List<String> foo1 = Arrays.asList("blah");
      |    public static List<String> foo = fo@@o1
      |}
      |""".stripMargin,
    "java/util/List# List.java",
  )

  check(
    "type-variable",
    """
      |import java.util.Arrays;
      |import java.util.List;
      |class A {
      |    public static <T extends CharSequence> int foo(T el) { return e@@l.length(); }
      |}
      |""".stripMargin,
    // Resolve to the upper bound of the type variable
    "java/lang/CharSequence# CharSequence.java",
  )

  check(
    "to-dependency",
    """
      |import com.google.common.collect.Range;
      |class A {
      |    public static Range<Integer> foo = Range.cl@@osed(1, 10);
      |}
      |""".stripMargin,
    // The type, not the "closed" method
    "com/google/common/collect/Range# Range.java",
  )

  check(
    "to-jdk-method",
    """
      |import java.util.Arrays;
      |import java.util.List;
      |class A {
      |    public static List<String> foo = Arrays.as@@List("blah");
      |}
      |""".stripMargin,
    "java/util/List# List.java",
  )

  check(
    "jdk-inner-class",
    """
      |class A {
      |    public static Object foo = java.util.Map.Entry.copy@@Of()
      |}
      |""".stripMargin,
    "java/util/Map#Entry# Map.java",
  )

  check(
    "ambiguous-sourcepath",
    """
      |class A {
      |    public static String valueOf(String s) { return s;}
      |    public static Integer valueOf(int i) { return i; }
      |    public static Object foo = A.va@@lueOf()
      |}
      |""".stripMargin,
    // The union of the return types
    """|java/lang/String# String.java
       |java/lang/Integer# Integer.java
       |""".stripMargin,
  )

  check(
    "ambiguous-field-sourcepath",
    """
      |class A {
      |    public static String valueOf(String s) { return s;}
      |    public static Integer valueOf(int i) { return i; }
      |    public static Object foo = A.va@@lueOf
      |}
      |""".stripMargin,
    """|java/lang/String# String.java
       |java/lang/Integer# Integer.java
       |""".stripMargin,
  )

  check(
    "ambiguous-classpath",
    """
      |class A {
      |    public static Object foo = String.copyValue@@Of()
      |}
      |""".stripMargin,
    // Distinct result even if there are multiple candidate methods, as long as
    // they have the same return type.
    "java/lang/String# String.java",
  )

  check(
    "basic",
    """|class A {
       |    public static Integer NUMBER = 42;
       |
       |    public static void main(String args[]){
       |        Integer x = NU@@MBER;
       |    }
       |}
       |""".stripMargin,
    "java/lang/Integer# Integer.java",
  )

  check(
    "local-variable",
    """|
       |class A {
       |    public static void main(String args[]){
       |        String x = 42;
       |        String y = @@x;
       |    }
       |}
       |""".stripMargin,
    "java/lang/String# String.java",
  )

  check(
    "method",
    """|class A {
       |    private static Double foo() {
       |       return 42;
       |    }
       |
       |    public static void main(String args[]){
       |      fo@@o();
       |    }
       |}
       |""".stripMargin,
    "java/lang/Double# Double.java",
  )

  check(
    "method-with-args",
    """|import java.util.List;
       |
       |class A {
       |    static Float foo(int x, String s) {
       |       return 42;
       |    }
       |
       |    public static void main(String args[]){
       |      fo@@o(1, "str");
       |    }
       |}
       |""".stripMargin,
    "java/lang/Float# Float.java",
  )

  check(
    "class-reference",
    """|class Foo {
       |    public int value = 42;
       |}
       |
       |class A {
       |    public static void main(String args[]){
       |      Fo@@o f = new Foo();
       |    }
       |}
       |""".stripMargin,
    """|Definition.java:2:7: info: definition
       |class Foo {
       |      ^^^
       |""".stripMargin,
  )

  check(
    "new-instance",
    """|class Foo {
       |    public int value = 42;
       |}
       |
       |class A {
       |    public static void main(String args[]){
       |      Foo f = new Fo@@o();
       |    }
       |}
       |""".stripMargin,
    "",
  )

  check(
    "field-access",
    """|class Foo {
       |    public Double value = 42;
       |}
       |
       |class A {
       |    public static void main(String args[]){
       |      Foo f = new Foo();
       |      int x = f.val@@ue;
       |    }
       |}
       |""".stripMargin,
    "java/lang/Double# Double.java",
  )

  check(
    "parameter",
    """|class A {
       |    static void foo(String x) {
       |       System.out.println(@@x);
       |    }
       |}
       |""".stripMargin,
    "java/lang/String# String.java",
  )

  check(
    "import",
    """|import java.util.@@List;
       |
       |class A {
       |    public static void main(String args[]){
       |      List<Integer> x = List.of(1);
       |    }
       |}
       |""".stripMargin,
    "java/util/List# List.java",
  )

  check(
    "extends",
    """|interface Foo {}
       |
       |class A implements Fo@@o {
       |}
       |""".stripMargin,
    """|Definition.java:2:11: info: definition
       |interface Foo {}
       |          ^^^
       |""".stripMargin,
  )

  check(
    "constructor-parameter",
    """|class Foo {
       |    private Double value;
       |    public Foo(Double value) {
       |        this.val@@ue = value;
       |    }
       |}
       |""".stripMargin,
    "java/lang/Double# Double.java",
  )

  check(
    "this-reference",
    """|class Foo {
       |    private Double value;
       |    public void setValue(int value) {
       |        this.val@@ue = value;
       |    }
       |}
       |""".stripMargin,
    "java/lang/Double# Double.java",
  )

  check(
    "static-import",
    """|import static java.lang.Math.max;
       |
       |class A {
       |    public static void main(String args[]){
       |      int x = m@@ax(1, 2);
       |    }
       |}
       |""".stripMargin,
    "java/lang/Math#max(). Math.java",
  )

  check(
    "enum-constant",
    """|enum Color {
       |    RED, GREEN, BLUE
       |}
       |
       |class A {
       |    public static void main(String args[]){
       |      Color c = Color.R@@ED;
       |    }
       |}
       |""".stripMargin,
    """|Definition.java:2:6: info: definition
       |enum Color {
       |     ^^^^^
       |""".stripMargin,
  )

  check(
    "method-overload",
    """|class A {
       |    static Integer foo(int x) {
       |       return 42;
       |    }
       |    static String foo(String s) {
       |       return "42";
       |    }
       |
       |    public static void main(String args[]){
       |      fo@@o(42);
       |    }
       |}
       |""".stripMargin,
    "java/lang/Integer# Integer.java",
  )

  check(
    "package",
    """|class A {
       |    public static void main(String args[]){
       |      int x = ja@@va.lang.Math.max(1, 2);
       |    }
       |}
       |""".stripMargin,
    "",
  )

}
