/*!
  \file %file%

  \brief %without-test%モジュールのテスト
*/
#include <CppUTest/TestHarness.h>
#include <CppUTest/TestRegistry.h>
#include <CppUTest/TestOutput.h>
#include <CppUTest/TestPlugin.h>
#include <%without-test%.h>

TEST_GROUP(%file-without-ext%Group)
{
  void setup()
  {
  }

  void teardown()
  {
  }
};

TEST(%file-without-ext%Group, MethodTest)
{
}
