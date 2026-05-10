package signatureHelp

import tests.pc.BaseJavaSignatureHelpSuite

class SignatureHelpSuite extends BaseJavaSignatureHelpSuite {

  check(
    "ambiguous-method",
    """
      |class A {
      |    public static void log(int i, String message) {}
      |    public static void log(String message) {}
      |    public void run() {
      |        A.log(1@@);
      |    }
      |}
      |""".stripMargin,
    """|=> log(int i, java.lang.String message)
       |       ^^^^^
       |   log(java.lang.String message)
       |""".stripMargin,
  )

  check(
    "constructor",
    """
      |class A {
      |    public A(String message) {}
      |    public void run() {
      |        new A(@@);
      |    }
      |}
      |""".stripMargin,
    """|=> A(java.lang.String message)
       |     ^^^^^^^^^^^^^^^^^^^^^^^^
       |""".stripMargin,
  )

  check(
    "valid-args",
    """package com.app;
      |
      |import java.io.IOException;
      |import java.nio.file.FileVisitResult;
      |import java.nio.file.SimpleFileVisitor;
      |import java.nio.file.attribute.BasicFileAttributes;
      |import java.nio.file.FileVisitResult;
      |import java.nio.file.Path;
      |import java.nio.file.Paths;
      |
      |public class Main {
      |	public static void blah() throws IOException {
      |		java.nio.file.Files.walkFileTree(java.nio@@.file.Paths.get(""), new SimpleFileVisitor<Path>() {
      |
      |			@Override
      |			public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
      |
      |				System.out.println(file);
      |				return FileVisitResult.CONTINUE;
      |			}
      |		});
      |	}
      |}
      |""".stripMargin,
    """|walkFileTree(java.nio.file.Path start, java.util.Set<java.nio.file.FileVisitOption> options, int maxDepth, java.nio.file.FileVisitor<? super java.nio.file.Path> visitor)
       |=> walkFileTree(java.nio.file.Path start, java.nio.file.FileVisitor<? super java.nio.file.Path> visitor)
       |                ^^^^^^^^^^^^^^^^^^^^^^^^
       |""".stripMargin,
  )

  check(
    "annotation",
    """
      |@Deprecated(since @@= , forRemoval = true)
      |public class A {
      |}
      |""".stripMargin,
    """|=> @Deprecated(java.lang.String since() default "", boolean forRemoval() default false)
       |               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       |""".stripMargin,
  )

  check(
    "chain",
    """
      |public class A {
      |    public A self(String message) { return this; }
      |    public void foo(String message) {
      |        new A().self("bla@@h").foo(String.format("blah %s", "foo"));
      |    }
      |}
      |""".stripMargin,
    """|=> self(java.lang.String message)
       |        ^^^^^^^^^^^^^^^^^^^^^^^^
       |""".stripMargin,
  )

  check(
    "custom-method",
    """
      |class Test {
      |  void myMethod(int x, String y) {}
      |  void test() {
      |    myMethod(1, @@);
      |  }
      |}
    """.stripMargin,
    """|=> myMethod(int x, java.lang.String y)
       |                   ^^^^^^^^^^^^^^^^^^
       |""".stripMargin,
  )

  check(
    "custom-constructor",
    """
      |class Test {
      |  Test(int x, String y) {}
      |  void create() {
      |    new Test(1, @@);
      |  }
      |}
    """.stripMargin,
    """|=> Test(int x, java.lang.String y)
       |               ^^^^^^^^^^^^^^^^^^
       |""".stripMargin,
  )

  check(
    "custom-constructor-empty",
    """
      |class Test {
      |  Test(int x, String y) {}
      |  void create() {
      |    new Test(@@);
      |  }
      |}
    """.stripMargin,
    """|=> Test(int x, java.lang.String y)
       |        ^^^^^
       |""".stripMargin,
  )

  check(
    "static-method",
    """
      |class Test {
      |  void test() {
      |    Math.max(1, @@);
      |  }
      |}
    """.stripMargin,
    """|=> max(int a, int b)
       |              ^^^^^
       |   max(long a, long b)
       |   max(float a, float b)
       |   max(double a, double b)
       |""".stripMargin,
  )

  check(
    "varargs",
    """
      |class Test {
      |  void test() {
      |    String.format("", 1, @@);
      |  }
      |}
    """.stripMargin,
    """|=> format(java.lang.String format, java.lang.Object[] args)
       |   format(java.util.Locale l, java.lang.String format, java.lang.Object[] args)
       |""".stripMargin,
  )

  check(
    "method-println",
    """
      |class Test {
      |  void test() {
      |    String name = "John";
      |    System.out.println(name@@);
      |  }
      |}
    """.stripMargin,
    """|   println()
       |   println(boolean x)
       |   println(char x)
       |   println(int x)
       |   println(long x)
       |   println(float x)
       |   println(double x)
       |   println(char[] x)
       |=> println(java.lang.String x)
       |           ^^^^^^^^^^^^^^^^^^
       |   println(java.lang.Object x)
       |""".stripMargin,
  )

  check(
    "method-println-empty",
    """
      |class Test {
      |  void test() {
      |    System.out.println(@@);
      |  }
      |}
    """.stripMargin,
    """|=> println()
       |   println(boolean x)
       |   println(char x)
       |   println(int x)
       |   println(long x)
       |   println(float x)
       |   println(double x)
       |   println(char[] x)
       |   println(java.lang.String x)
       |   println(java.lang.Object x)
       |""".stripMargin,
  )

  check(
    "method-second-param",
    """
      |class Test {
      |  void test() {
      |    String.format("", @@);
      |  }
      |}
    """.stripMargin,
    """|=> format(java.lang.String format, java.lang.Object[] args)
       |                                   ^^^^^^^^^^^^^^^^^^^^^^^
       |   format(java.util.Locale l, java.lang.String format, java.lang.Object[] args)
       |""".stripMargin,
  )

  check(
    "method-first-param",
    """
      |class Test {
      |   public void setVariable(final String name, final Object value) {
      |       // this.variables.put(name, value);
      |  }
      |  void test() {
      |    setVariable(@@);
      |  }
      |}
    """.stripMargin,
    """|=> setVariable(java.lang.String name, java.lang.Object value)
       |               ^^^^^^^^^^^^^^^^^^^^^
       |""".stripMargin,
  )

  check(
    "constructor",
    """
      |class Test {
      |  void test() {
      |    new String(@@);
      |  }
      |}
    """.stripMargin,
    """|=> String()
       |   String(java.lang.String original)
       |   String(char[] value)
       |   String(char[] value, int offset, int count)
       |   String(int[] codePoints, int offset, int count)
       |   String(byte[] ascii, int hibyte, int offset, int count)
       |   String(byte[] ascii, int hibyte)
       |   String(byte[] bytes, int offset, int length, java.lang.String charsetName)
       |   String(byte[] bytes, int offset, int length, java.nio.charset.Charset charset)
       |   String(byte[] bytes, java.lang.String charsetName)
       |   String(byte[] bytes, java.nio.charset.Charset charset)
       |   String(byte[] bytes, int offset, int length)
       |   String(byte[] bytes)
       |   String(java.lang.StringBuffer buffer)
       |   String(java.lang.StringBuilder builder)
       |   String(char[] value, int off, int len, java.lang.Void sig)
       |   String(java.lang.AbstractStringBuilder asb, java.lang.Void sig)
       |   String(byte[] value, byte coder)
       |""".stripMargin,
  )

  check(
    "constructor-second-param",
    """
      |import java.io.File;
      |class Test {
      |  void test() {
      |    new File("test", @@);
      |  }
      |}
    """.stripMargin,
    """|   File(java.lang.String pathname, int prefixLength)
       |   File(java.lang.String child, java.io.File parent)
       |   File(java.lang.String pathname)
       |=> File(java.lang.String parent, java.lang.String child)
       |                                 ^^^^^^^^^^^^^^^^^^^^^^
       |   File(java.io.File parent, java.lang.String child)
       |   File(java.net.URI uri)
       |""".stripMargin,
  )

  check(
    "empty-args",
    """
      |class Test {
      |  void myMethod(String x) {}
      |  void myMethod(Integer x) {}
      |  void test() {
      |    myMethod(@@);
      |  }
      |}
    """.stripMargin,
    """|=> myMethod(java.lang.String x)
       |            ^^^^^^^^^^^^^^^^^^
       |   myMethod(java.lang.Integer x)
       |""".stripMargin,
  )

  check(
    "nested",
    """
      |class Test {
      |  void test() {
      |    System.out.println(String.valueOf(@@));
      |  }
      |}
    """.stripMargin,
    """|=> valueOf(java.lang.Object obj)
       |           ^^^^^^^^^^^^^^^^^^^^
       |   valueOf(char[] data)
       |   valueOf(char[] data, int offset, int count)
       |   valueOf(boolean b)
       |   valueOf(char c)
       |   valueOf(int i)
       |   valueOf(long l)
       |   valueOf(float f)
       |   valueOf(double d)
       |""".stripMargin,
  )

  check(
    "nested-outer",
    """
      |class Test {
      |  void test() {
      |    System.out.println(Str@@ing.valueOf(12));
      |  }
      |}
    """.stripMargin,
    """|   println()
       |   println(boolean x)
       |   println(char x)
       |   println(int x)
       |   println(long x)
       |   println(float x)
       |   println(double x)
       |   println(char[] x)
       |=> println(java.lang.String x)
       |           ^^^^^^^^^^^^^^^^^^
       |   println(java.lang.Object x)
       |""".stripMargin,
  )
}
