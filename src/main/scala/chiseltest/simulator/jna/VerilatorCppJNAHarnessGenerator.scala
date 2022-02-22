// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator.jna

import chiseltest.simulator.{PinInfo, TopmoduleInfo}

/** Generates the Module specific verilator harness cpp file for verilator compilation.
  *  This version generates a harness that can be called into through the JNI.
  */
private[chiseltest] object VerilatorCppJNAHarnessGenerator {
  def codeGen(
    toplevel:     TopmoduleInfo,
    vcdFilePath:  os.Path,
    targetDir:    os.Path,
    majorVersion: Int,
    minorVersion: Int,
    verbose:      Boolean,
    appName:      String
  ): String = {
    val pokeable = toplevel.inputs.zipWithIndex
    val peekable = (toplevel.inputs ++ toplevel.outputs).zipWithIndex
    def fitsIn64Bits(s: (PinInfo, Int)): Boolean = s._1.width <= 64
    def fitsInLongBits(s: (PinInfo, Int)): Boolean = s._1.width > 64
    
    def getType(width:Int, signed:Boolean):String={
      var s_signed= 
      if(signed==true || width==1)
          ""
      else
          "unsigned "
      val s_type=
      if (width==1)
          "bool"
      else if(width<=32){
          "int"
      }
      else if(width<=64){
          "long"
      }
      else{
          s_signed = ""
          "sc_bv<"+width.toString+">"
      }
      s_signed+s_type
    }
    val codeBuffer = new StringBuilder
    // generate Verilator specific "sim_state" class
    codeBuffer.append(s"""
struct sim_state {
  TOP_CLASS* dut;
""")
  if(!appName.equals("")) codeBuffer.append(s"  cfm_${appName}app* cfmi_CofluentApp;\n")
codeBuffer.append(s"""
  VERILATED_C* tfp;
  vluint64_t main_time;
""")
  if(!appName.equals("")) codeBuffer.append(s"""  sc_clock clk{"clk", 1, SC_NS, 0.5, 3, SC_NS, true};\n""")

  peekable.filter(fitsIn64Bits).foreach { case (PinInfo(name, width, signed), id) =>
      val s_type = getType(width, signed)
      if(!appName.equals("")) codeBuffer.append(s"  sc_signal<$s_type> ${name};\n")
  }
  peekable.filter(fitsInLongBits).foreach { case (PinInfo(name, width, signed), id) =>
      val s_type = getType(width, signed)
      if(!appName.equals("")) codeBuffer.append(s"  sc_signal<$s_type> ${name};\n")
  }
codeBuffer.append(s"""
  sim_state() :
    dut(new TOP_CLASS("dut")),
""")
    if(!appName.equals("")) codeBuffer.append(s"""    cfmi_CofluentApp(new cfm_${appName}app("CofluentApp")),\n""")
codeBuffer.append(s"""
    tfp(nullptr),
    main_time(0)
  {
    // std::cout << "Allocating! " << ((long long) dut) << std::endl;
""")
    if(!appName.equals("")) codeBuffer.append(s"    dut->clock(clk);\n")

    peekable.filter(fitsIn64Bits).foreach { case (PinInfo(name, _, _), id) =>
      if(!appName.equals("")) codeBuffer.append(s"    dut->${name}($name);\n")
    }
    peekable.filter(fitsInLongBits).foreach { case (PinInfo(name, _, _), id) =>
      if(!appName.equals("")) codeBuffer.append(s"    dut->${name}($name);\n")
    }
codeBuffer.append(s"""
  }

  inline int64_t step(int32_t cycles) {
    for(int32_t i = 0; i < cycles; i++) {
      const int64_t status = _step(tfp, dut, main_time);
      if(status > 0) {
        // early exit on failure
        return (status << 32) | ((int64_t)(i + 1));
      }
    }
    return (int64_t)cycles;
  }
  inline void update() { }//sc_start(1,SC_NS);
  inline void finish() {
""")
    if(!appName.equals("")) 
        codeBuffer.append(s"    _finish(tfp, dut,cfmi_CofluentApp);\n")
    else
        codeBuffer.append(s"    _finish(tfp, dut);\n")
codeBuffer.append(s"""
  }
  inline void resetCoverage() { VerilatedCov::zero(); }
  inline void writeCoverage(const char* filename) {
    VerilatedCov::write(filename);
  }
  inline void poke(int32_t id, int64_t value) {
    const uint64_t u = value;
    //std::cout << "poking: " << std::hex << u <<",id:"<<id<< std::endl;
    switch(id) {
""")
    pokeable.filter(fitsIn64Bits).foreach { case (PinInfo(name, _, _), id) =>
      if(!appName.equals("")) codeBuffer.append(s"      case $id : ${name}.write(value);break;\n")
      else codeBuffer.append(s"      case $id : dut->$name = u; break;\n")
    }
    codeBuffer.append(s"""
    default:
      std::cerr << "Cannot find the object of id = " << id << std::endl;
      finish();
      break;
    }
  }
  inline int64_t peek(int32_t id) {
    uint64_t value = 0;
    switch(id) {
""")
    peekable.filter(fitsIn64Bits).foreach { case (PinInfo(name, _, _), id) =>
      if(!appName.equals("")) codeBuffer.append(s"      case $id : value = ${name}.read(); break;\n")
      else codeBuffer.append(s"      case $id : value = dut->$name; break;\n")
    }
    codeBuffer.append(s"""
    default:
      std::cerr << "Cannot find the object of id = " << id << std::endl;
      finish();
      return -1;
    }
    // std::cout << "peeking: " << std::hex << value << std::endl;
    return value;
  }
""")
if(!appName.equals("")) {
codeBuffer.append(s"""
  inline void poke_wide(int32_t id, int32_t offset, int64_t value) {}
""") 
}
else{
codeBuffer.append(s"""
  inline void poke_wide(int32_t id, int32_t offset, int64_t value) {
    const uint64_t u = value;
    WData* data = nullptr;
    size_t words = 0;
    switch(id) {
""")
    pokeable.filterNot(fitsIn64Bits).foreach { case (PinInfo(name, width, _), id) =>
      val numWords = (width - 1) / 32 + 1
      codeBuffer.append(s"      case $id : data = dut->$name; words = $numWords; break;\n")
    }
    codeBuffer.append(s"""
    default:
      std::cerr << "Cannot find the object of id = " << id << std::endl;
      finish();
      break;
    }
    const size_t firstWord = offset * 2;
    const size_t secondWord = firstWord + 1;
    if(firstWord >= words || firstWord < 0) {
      std::cerr << "Out of bounds index for id = " << id << " index = " << offset << std::endl;
      finish();
    } else if(secondWord >= words) {
      data[firstWord] = u;
    } else {
      data[firstWord] = u & 0xffffffffu;
      data[secondWord] = (u >> 32) & 0xffffffffu;
    }
  }
""")
}
codeBuffer.append(s"""
  inline void poke2(int32_t id, const char* value) {
    sc_biguint<256> v(value);
    switch(id) {
""")
    pokeable.filter(fitsInLongBits).foreach { case (PinInfo(name, width, _), id) =>
      codeBuffer.append(s"      case $id : ${name}.write(v);break;\n")
    }
    codeBuffer.append(s"""
    default:
      std::cerr << "Cannot find the object of id = " << id << std::endl;
      finish();
      break;
    }
  }
""")
if(!appName.equals("")) {
codeBuffer.append(s"""
  inline const char* peek2(int32_t id) {
    sc_bv<256> value = 0;
    int32_t remaining = 0;
    switch(id) {
""")
    peekable.filter(fitsInLongBits).foreach { case (PinInfo(name, width, _), id) =>
      codeBuffer.append(s"      case $id : value = ${name}.read(); \n               remaining = 256-${width};\n               value = (value<<remaining)>>remaining;\n               break;\n")
    }
    codeBuffer.append(s"""
    default:
      std::cerr << "Cannot find the object of id = " << id << std::endl;
      finish();
      return "-1";
    }
    std::string ss = value.to_string();
    return ss.c_str();
  }

  inline int64_t peek_wide(int32_t id, int32_t offset) {
    sc_bv<256> value(0);
    size_t words = 0;
    switch(id) {
""")
    peekable.filter(fitsInLongBits).foreach { case (PinInfo(name, width, _), id) =>
      val numWords = (width - 1) / 32 + 1
      //codeBuffer.append(s"      case $id : value = ${name}.read(); \n               words = $numWords;\n               remaining = 256-${width};\n               value = (value<<remaining)>>remaining;\n               break;\n")
      codeBuffer.append(s"      case $id : value = ${name}.read(); words = $numWords; break;\n")
      }
codeBuffer.append(s"""
    default:
      std::cerr << "Cannot find the object of id = " << id << std::endl;
      finish();
      return -1;
    }
    const size_t firstWord = offset * 2;
    const size_t secondWord = firstWord + 1;
    if(firstWord >= words || firstWord < 0) {
      std::cerr << "Out of bounds index for id = " << id << " index = " << offset << std::endl;
      finish();
      return -1;
    } else if(secondWord >= words) {
      return (uint64_t)value.get_word(firstWord);
    } else {
      //std::cout<<"zjp2 value else:"<<hex<<value<<",data:"<<hex<<((((uint64_t)value.get_word(secondWord)) << 32) | ((uint64_t)value.get_word(firstWord)))<<std::endl;
      return (((uint64_t)value.get_word(secondWord)) << 32) | ((uint64_t)value.get_word(firstWord));
    }
  }
""") 
}
else{
codeBuffer.append(s"""
  inline int64_t peek_wide(int32_t id, int32_t offset) {
    WData* data = nullptr;
    size_t words = 0;
    switch(id) {
""")
    peekable.filterNot(fitsIn64Bits).foreach { case (PinInfo(name, width, _), id) =>
      val numWords = (width - 1) / 32 + 1
      codeBuffer.append(s"      case $id : data = dut->$name; words = $numWords; break;\n")
    }
codeBuffer.append(s"""
    default:
      std::cerr << "Cannot find the object of id = " << id << std::endl;
      finish();
      return -1;
    }
    const size_t firstWord = offset * 2;
    const size_t secondWord = firstWord + 1;
    if(firstWord >= words || firstWord < 0) {
      std::cerr << "Out of bounds index for id = " << id << " index = " << offset << std::endl;
      finish();
      return -1;
    } else if(secondWord >= words) {
      return (uint64_t)data[firstWord];
    } else {
      return (((uint64_t)data[secondWord]) << 32) | ((uint64_t)data[firstWord]);
    }
  }
""")
}
codeBuffer.append(s"""
  inline void set_args(int32_t argc, const char** argv) {
    Verilated::commandArgs(argc, argv);
  }
};

static sim_state* create_sim_state() {
  sim_state *s = new sim_state();
  std::string dumpfile = "${vcdFilePath}";
  _startCoverageAndDump(&s->tfp, dumpfile, s->dut);
  return s;
}
""")

    val jnaCode = JNAUtils.genJNACppCode(codeBuffer.toString())
    commonCodeGen(toplevel, targetDir, majorVersion, minorVersion, verbose, appName) + jnaCode
  }

  private def commonCodeGen(
    toplevel:     TopmoduleInfo,
    targetDir:    os.Path,
    majorVersion: Int,
    minorVersion: Int,
    verbose:      Boolean,
    appName:      String
  ): String = {
    val dutName = toplevel.name
    val dutVerilatorClassName = "V" + dutName

    toplevel.requireNoMultiClock()
    val clockName = toplevel.clocks.headOption
    val clockLow = clockName.map("top->" + _ + " = 0;").getOrElse("")
    val clockHigh = clockName.map("top->" + _ + " = 1;").getOrElse("")

    val coverageInit =
      if (majorVersion >= 4 && minorVersion >= 202)
        """|#if VM_COVERAGE
           |    Verilated::defaultContextp()->coveragep()->forcePerInstance(true);
           |#endif
           |""".stripMargin
      else ""

    val verilatorRunFlushCallback = if (majorVersion >= 4 && minorVersion >= 38) {
      "Verilated::runFlushCallbacks();" // Verilated::runExitCallbacks();
    } else {
      "Verilated::flushCall();"
    }

    val codeBuffer = new StringBuilder
    codeBuffer.append(s"""#include "$dutVerilatorClassName.h"
                         |#include "verilated.h"
                         |#include <sysc/datatypes/int/sc_bigint.h>
                         |#include <sysc/datatypes/bit/sc_bv.h>
                         |
                         |#define TOP_CLASS $dutVerilatorClassName
                         |
                         |#ifndef VM_TRACE_FST
                         |#define VM_TRACE_FST 0
                         |#endif
                         |
                         |static const bool verbose = $verbose;
                         |
                         |#if VM_TRACE
                         |#if VM_TRACE_FST
                         |  #include "verilated_fst_sc.h"
                         |  #define VERILATED_C VerilatedFstC
                         |#else // !(VM_TRACE_FST)
                         |  #include "verilated_vcd_sc.h"
                         |  #define VERILATED_C VerilatedVcdC
                         |#endif
                         |#else // !(VM_TRACE)
                         |  #define VERILATED_C VerilatedVcdC
                         |#endif
                         |#include <iostream>
    |""".stripMargin)
                         if(!appName.equals("")) 
                             codeBuffer.append(s"""|#include "cfm_${appName}app_top.h"\n""".stripMargin)
    codeBuffer.append(s"""
                         |
                         |
                         |// Override Verilator definition so first $$finish ends simulation
                         |// Note: VL_USER_FINISH needs to be defined when compiling Verilator code
                         |static bool encounteredFinish = false;
                         |void vl_finish(const char* filename, int linenum, const char* hier) {
                         |  // std::cout << "finish! (" << filename << ", " << linenum << ", " << hier << ")" << std::endl;
                         |  $verilatorRunFlushCallback
                         |  encounteredFinish = true;
                         |}
                         |
                         |
                         |static bool encounteredFatal = false;
                         |void vl_fatal(const char* filename, int linenum, const char* hier, const char* msg) {
                         |  std::cerr << "fatal! (" << filename << ", " << linenum << ", " << hier << ", " << msg << ")" << std::endl;
                         |  $verilatorRunFlushCallback
                         |  encounteredFatal = true;
                         |}
                         |
                         |
                         |static bool encounteredStop = false;
                         |void vl_stop(const char* filename, int linenum, const char* hier) {
                         |  // std::cout << "stop! (" << filename << ", " << linenum << ", " << hier << ")" << std::endl;
                         |  $verilatorRunFlushCallback
                         |  encounteredStop = true;
                         |}
                         |
                         |
                         |// required for asserts (until Verilator 4.200)
                         |double sc_time_stamp () { return 0; }
                         |
                         |static void _startCoverageAndDump(VERILATED_C** tfp, const std::string& dumpfile, TOP_CLASS* top) {
                         |$coverageInit
                         |#if VM_TRACE || VM_COVERAGE
                         |    Verilated::traceEverOn(true);
                         |#endif
                         |#if VM_TRACE
                         |    if (verbose) VL_PRINTF(\"Enabling waves..\\n\");
                         |    *tfp = new VerilatedVcdSc;
                         |    top->trace(*tfp, 99);
                         |    (*tfp)->open(dumpfile.c_str());
                         |#endif
                         |}
                         |
                         |static int64_t _step(VERILATED_C* tfp, TOP_CLASS* top, vluint64_t& main_time) {
    |""".stripMargin)
                         if(appName.equals("")) codeBuffer.append(s"""|    $clockLow\n|    top->eval();\n""".stripMargin)
    codeBuffer.append(s"""           
                         |#if VM_TRACE
                         |    if (tfp) tfp->dump(main_time);
                         |#endif
                         |    main_time++;
    |""".stripMargin)
                         if(appName.equals("")) codeBuffer.append(s"""|    $clockHigh\n|    top->eval();\n""".stripMargin)
                         else codeBuffer.append(s"""|    sc_start(1,SC_NS);\n""".stripMargin)
    codeBuffer.append(s""" 
                         
                         |#if VM_TRACE
                         |    if (tfp) tfp->dump(main_time);
                         |#endif
                         |    main_time++;
                         |    if(encounteredStop) {
                         |      // vl_stop is called by verilator when an assertion fails or when the fatal command is executed
                         |      encounteredStop = false;
                         |      encounteredFinish = false;
                         |      return 2;
                         |    } else if(encounteredFinish) {
                         |      // vl_finish is called by verilator when a finish command is executed (stop(0))
                         |      encounteredFinish = false;
                         |      return 1;
                         |    } else if(encounteredFatal) {
                         |      encounteredFatal = false;
                         |      return 3;
                         |    }
                         |    return 0;
                         |}
                         |
    |""".stripMargin)
                         if(!appName.equals("")) 
                             codeBuffer.append(s"|static void _finish(VERILATED_C* tfp, TOP_CLASS* top, cfm_${appName}app* app) {\n".stripMargin)
                         else
                             codeBuffer.append(s"|static void _finish(VERILATED_C* tfp, TOP_CLASS* top) {\n".stripMargin)
    codeBuffer.append(s"""
                         |#if VM_TRACE
                         |  if (tfp) tfp->close();
                         |  delete tfp;
                         |#endif
                         |#if VM_COVERAGE
                         |  VerilatedCov::write("$targetDir/coverage.dat");
                         |#endif
                         |  top->final();
                         |  // TODO: re-enable!
                         |  // delete top;
    |""".stripMargin)
                         if(!appName.equals("")) codeBuffer.append(s"|delete app;\n".stripMargin)
                         codeBuffer.append(s"""
                         |}
    |""".stripMargin)

    codeBuffer.toString()
  }
}
