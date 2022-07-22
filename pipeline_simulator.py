# 5 Stage MIPS Simulator
# NOTE: It deals with data hazards. It will also deal with branch hazard if the input is given in a specific manner.


# ---------------------------------------------------------------------------------------------------------------------------------
# Initial Value Assignment

# Registers
rs = [0, 0, 0, 0]  # WB | MEM | EX | DECODE stage
rt = [0, 0, 0, 0]  # WB | MEM | EX | DECODE stage
rd = [0, 0, 0, 0]  # WB | MEM | EX | DECODE stage
funct = [0, 0]  # EX | DECODE stage
shamt = [0, 0]  # EX | DECODE stage
op = [0, 0]  # EX | DECODE stage
Branch = [0, 0]  # EX | DECODE stage
branch_target = 0  # EX

# Control signals
RegDst = [0, 0, 0, 0]  # WB | MEM | EX | DECODE stage
MemToReg = [0, 0, 0, 0]  # WB | MEM | EX | DECODE stage
RegWrite = [0, 0, 0, 0]  # WB | MEM | EX | DECODE stage
MemRead = [0, 0, 0]  # MEM | EX | DECODE stage
MemWrite = [0, 0, 0, 0]  # WB| MEM | EX | DECODE stage
ALUOp = [0, 0]   # EX | DECODE stage
ALUSrc = [0, 0]  # EX | DECODE stage

inst_assembly = [0, 0, 0, 0, 0]  # used for printing the instruction

# Latches
IF_ID = [0, 0]  # Current instruction | Next instruction
# Current and Next READ REG1 | READ REG2 | SIGN EXTEND
ID_EX = [[0, 0, 0], [0, 0, 0]]
EX_MEM = [[0, 0], [0, 0]]  # Current and Next ALU result | alu_sr2
MEM_WB = [[0, 0], [0, 0]]  # Current and Next MEMORY_READ_DATA | ALU_result

# registers: register number, stored value, register name, what does the register do.
regs = [[0, 0, "$zero"], [1, 0, "$at"], [2, 0, "$v0"], [3, 0, "$v1"], [4, 0, "$a0"], [5, 0, "$a1"], [6, 0, "$a2"], [7, 0, "$a3"], [8, 0, "$t0"],
        [9, 0, "$t1"], [10, 0, "$t2"], [11, 0, "$t3"], [12, 0, "$t4"], [
            13, 0, "$t5"], [14, 0, "$t6"], [15, 0, "$t7"], [16, 0, "$s0"], [17, 0, "$s1"],
        [18, 0, "$s2"], [19, 0, "$s3"], [20, 0, "$s4"], [21, 0, "$s5"], [
            22, 0, "$s6"], [23, 0, "$s7"], [24, 0, "$t8"], [25, 0, "$t9"],
        [26, 0, "$k0"], [27, 0, "$k1"], [28, 0, "$gp"], [29, 0, "$sp"], [30, 0, "$fp"], [31, 0, "$ra"]]

LO_REG = 0  # multipication first stored in low, when overflow it is in high

# Initialize Memory
inst_mem = []
data_mem = [1, 2, 3, 4, 5, 6, 0, 0]  # matA= [1,2],[3,4] matX=[4,5], matB=[0,0]


# Encoding
ALU = {0b0000: lambda src1, src2: ["and", src1 & src2, "bitwise and"],
       0b0001: lambda src1, src2: ["or",  src1 | src2, "bitwise or"],
       0b1000: lambda src1, src2: ["ori",  src1 | src2, "bitwise ori"],
       0b0010: lambda src1, src2: ["add", src1 + src2, "add signed"],
       0b0011: lambda src1, src2: ["sll", (src1 * (2**src2)), "shift logical left"],
       0b0110: lambda src1, src2: ["sub", src1 - src2, "sub signed"],
       0b0111: lambda src1, src2: ["slt", 0, "set on less than"],
       0b1100: lambda src1, src2: ["nor", ~(src1 | src2), "bitwise nor"],
       0b1101: lambda src1, src2: ["multu", src1*src2, "multiply"],
       0b1110: lambda src1, src2: ["mflo", LO_REG, "move from LO_REG"]}

# standard encoding
decode_funct = {0b000000: ["sll", 0b0011],
                0b100000: ["add", 0b0010],
                0b100010: ["sub", 0b0110],
                0b100100: ["and", 0b0000],
                0b100101: ["or",  0b0001],
                0b101010: ["slt", 0b0111],
                0b011001: ["multu", 0b1101],
                0b010010: ["mflo", 0b1110]}

decode_Ifunct = {0b001101: ["ori", 0b1000],  # 0b001101 : ["or", 0b0001] error
                 0b001000: ["addi", 0b0010]}

BranchAddress = {0b000100: lambda Zero, greatherThanZero: (
    branch_target if (Zero) else PC_plus_4)}  # beq
# RegDst -- Destination Register selection based upon R or I Format
# ALUSrc -- ALU operand from either register file or sign extended immediate
# MemToReg -- Loads - selects register write data from memory
# RegWrite -- Write Enable for Register File
# MemRead -- Read Enable for Data Memory
# MemWrite -- Write Enable for Data Memory
# Branch -- Branch instruction used to qualify next PC address
# ALUOp -- ALU operation predecode
# | RegDst | ALUSrc | MemToReg | RegWrite | MemRead | MemWrite | Branch | ALUOp |


# Assigning signals to their respective controls:
control = {0b000000: [1, 0, 0, 1, 0, 0, 0, 2],  # R Format
           0b100011: [0, 1, 1, 1, 1, 0, 0, 0],  # lw
           0b101011: [0, 1, 0, 0, 0, 1, 0, 0],  # sw
           0b000100: [0, 0, 0, 0, 0, 0, 1, 1],  # beq
           0b001101: [0, 1, 0, 1, 0, 0, 0, 3],  # ori
           0b001000: [0, 1, 0, 1, 0, 0, 0, 3]}  # addi


# --------------------------------------------------------------------------------------------------------------------------------
# Function Definitions

def display_regs():  # prints the data with name of registers
    x = [x*2 for x in range(16)]
    for i in x:
        print(regs[i][2] + "=", "%#010x" % (regs[i][1])+'    ' +
              regs[i+1][2] + "=", "%#010x" % (regs[i+1][1]))
    return

# control signals comparing according to assignment


def ALU_control(ALUOp, funct, opcode):
    if (ALUOp == 0):  # lw, sw => add
        return(0b0010)
    if (ALUOp == 1):  # beq => sub
        return(0b0110)
    if (ALUOp == 2):  # Rtype
        return (decode_funct[funct][1])
    if ALUOp == 3:
        return (decode_Ifunct[opcode][1])

# reading instructions from instruction.text


def read_instr():
    infile = open("instructions.txt", "r")
    lines = infile.readlines()
    # Ignoring the comments from the text.
    codelines = [x for x in lines if x[0] != "#"]
    for line in codelines:
        words = line.split()
        mem = (int(words[0], 16), int(words[1], 16), words[2])
        inst_mem.append(mem)
    infile.close()
    return


# ---------------------------------------------------------------------------------------------------------------------------------
#

read_instr()  # Read Instruction Memory

PC = 0
clock = 0  # a variable holding clock counts
Zero = 0
greaterThanZero = 0

while True:
    ### FETCH STAGE ###
    # fetch instruction
    try:                                            # codes inside the try will fail if all the instructions are executed
        addr = inst_mem[PC >> 2][0]  # PC counter
        inst_assembly[4] = inst_mem[PC >> 2][2]  # Machine code instruction
        instruction = inst_mem[PC >> 2][1]  # Instruction in MIPS
    except:                                         # it will only run if there are no more instructions in the txt file
        # all the instructions are executed because there are many zero
        break
        # instructions after the actual instructions

    # latch result of this stage into its pipeline reg
    IF_ID[1] = instruction

    clock = clock + 1

    PC_plus_4 = PC + 4
    PC = PC_plus_4

    print('----------------------------------------------------')
    print('/F\ Fetched = ' + str(inst_assembly[4])+    '  ------Clock cycle = '+ str(clock))
    print('IF/ID ----- To decode = '+ "%#010x"%(IF_ID[0])+'  fetched = '+ "%#010x" %(IF_ID[1]))

    ### DECODE STAGE ###
    if (clock >= 2):
        ## WRITE FIRST THEN READ##
        if (clock >= 5):
            if MemToReg[0]:  # if this control signal is 1 then it is lw and it will writeback mem_wb[0][0] which is memory data
                register_write_data = MEM_WB[0][0]  # register write back

            else:  # else write back alu result
                register_write_data = MEM_WB[0][1]  # register write back

            if RegDst[0]:  # it is R-type
                write_register = rd[0]
            else:
                write_register = rt[0]  # it is I-type

            # do not write to reg zero. FOR MULTU
            if (RegWrite[0] and (write_register != 0)):
                regs[write_register][1] = register_write_data

        # decode instruction
        # temp = str(IF_ID)
        # op[1] = int(temp[0:5],2)

        op[1] = IF_ID[0] >> 26  # first 6 bits - opcode
        rs[3] = (IF_ID[0] >> 21) & 0x1F  # shift to get rs
        # store in next3 since it is used in WB. shift to get rt
        rt[3] = (IF_ID[0] >> 16) & 0x1F
        rd[3] = (IF_ID[0] >> 11) & 0x1F  # shift to get rd
        shamt[1] = (IF_ID[0] >> 6) & 0x1F  # shift to get shamt
        funct[1] = IF_ID[0] & 0x3F  # shift to get funct
        my_imm = IF_ID[0] & 0xFFFF              #

        # control signals
        control_word = control[op[1]]
        ALUSrc[1] = control_word[1]
        RegDst[3] = control_word[0]
        MemToReg[3] = control_word[2]
        RegWrite[3] = control_word[3]
        MemRead[2] = control_word[4]
        MemWrite[3] = control_word[5]
        Branch[1] = control_word[6]
        ALUOp[1] = control_word[7]

        # register file sources
        read_data1 = regs[rs[3]][1]
        read_data2 = regs[rt[3]][1]

        # sign extension of immediate data
        sign_bit = (my_imm >> 15) & 0x1
        if (sign_bit == 1):
            sign_ext = (my_imm-(0x10000))
        else:
            sign_ext = my_imm

        # Latch results of that stage into its pipeline reg
        ID_EX[1] = [read_data1, read_data2, sign_ext]
        print('Decoded: ' + str(inst_assembly[3]), '\n')
        print("ID/EX | for current execute= [%d, %d, %d]" % ((ID_EX[0][0]), (ID_EX[0][1]), (ID_EX[0][2])),
              " result of current decode =[%d, %d, %d]" % ((ID_EX[1][0]), (ID_EX[1][1]), (ID_EX[1][2])))
        print("RS =  [%d, %d, %d, %d]" % ((rs[0]), (rs[1]), (rs[2]), (rs[3])))
        print("RT =  [%d, %d, %d, %d]" % ((rt[0]), (rt[1]), (rt[2]), (rt[3])))
        print("RD = [%d, %d, %d, %d]" % ((rd[0]), (rd[1]), (rd[2]), (rd[3])))

        print("MemRead =    ", MemRead)
        print("MemWrite = ", MemWrite)
        print("MemToReg = ", MemToReg)
        print("RegWrite = ", RegWrite)
        print("regDst =   ", RegDst)

    ### EXECUTE STAGE ###
    if (clock >= 3):
        # alu sources when there is no hazard
        alu_src1 = ID_EX[0][0]     # read_data1

        if ALUSrc[0]:
            # if ALUsrc_current = 1 then sign Extend is alu_src2 else read_data2. I-Type
            alu_src2 = ID_EX[0][2]
        else:
            alu_src2 = ID_EX[0][1]  # R-type

        readData2 = ID_EX[0][1]

        # if RD of WB stage (RD[0]) == Ex stage's Rs[2] or RT[2] then hazard is present         (R.A.W.) hazard
        # if RD of MEM stage (RD[1]) == Ex stage's RS[2] or RT[2] then hazard is present        (R.A.W.)

        #
        hazardDetected_rs = 0
        #
        hazardDetected_rt = 0

        #
        if (clock > 5):
            # check if there is a write back to register
            if ((RegWrite[0]) or (RegWrite[1])):
                #
                if ((MemWrite[0] == 0) and (MemWrite[1] == 0) and (MemWrite[2] == 0)):
                    # check if intruction in WB or MEM writes back to register and WB,MEM,EX is not a sw inst       #
                    if RegDst[0]:
                        WB_hazard_RD_check = rd[0]  # r-type
                    else:
                        WB_hazard_RD_check = rt[0]   # i-type

                    if (WB_hazard_RD_check == rs[2]):  # with RS
                        if((ALUOp[0] == 3)):  # if I instruction
                            print("RS forwaded from WB stage. RS = %d" %
                                  (MEM_WB[0][1]))          #
                            # load into alu_src1 the value of ALU result                        #
                            alu_src1_rs = MEM_WB[0][1]
                            hazardDetected_rs = 1
                        else:
                            print("RS forwaded from WB stage. RS = %d" %
                                  (MEM_WB[0][0]))       #
                            # load into alu_src1 the value of the loaded data from memory   #
                            alu_src1_rs = MEM_WB[0][0]
                            hazardDetected_rs = 1

                    if (WB_hazard_RD_check == rt[2]):  # with RT
                        if((ALUOp[0] == 3)):  # if I instruction
                            alu_src2 = alu_src2
                        else:
                            print("RT forwaded from WB stage. RT = %d" %
                                  (MEM_WB[0][0]))       #
                            #
                            alu_src2_rt = MEM_WB[0][0]
                            hazardDetected_rt = 1

                    if RegDst[1]:
                        MEM_hazard_RD_check = rd[1]
                    else:
                        MEM_hazard_RD_check = rt[1]
                        #
                    if (MEM_hazard_RD_check == rs[2]):  # with RS
                        print("RS forwaded from MEM stage. RS = %d" %
                              (EX_MEM[0][0]))          #
                        # load into alu_src1 the value of ALU result                        #
                        alu_src1_rs = EX_MEM[0][0]
                        hazardDetected_rs = 1

                    # with RT AND source is a reg not signext#
                    if ((MEM_hazard_RD_check == rt[2]) and (ALUSrc[0] == 0)):
                        print("RT forwaded from MEM stage. RT = %d" %
                              (EX_MEM[0][0]))          #
                        #
                        alu_src2_rt = EX_MEM[0][0]
                        #
                        hazardDetected_rt = 1

        if hazardDetected_rs == 1:  # fetch the forwaded data
            alu_src1 = alu_src1_rs
        if hazardDetected_rt == 1:
            alu_src2 = alu_src2_rt

        print("AluSrc = ", ALUSrc)
        print("ALUOp = ", ALUOp)
        print("funct = ", funct)
        print("shamt = ", shamt)
        print("ALU SOURCES =", alu_src1, alu_src2)

        # check if there is a sll inst
        sll = ((ALUOp[0] == 2) and (funct[0] == 0b000000))
        if sll:  # if sll then alu_src2 = sign ext
            alu_src1 = alu_src2
            alu_src2 = shamt[0]
            print("SOURCES UPDATED___ALU SOURCES =", alu_src1, alu_src2)

        # ALUOp is the current instruction
        alu_operation = ALU_control(ALUOp[0], funct[0], op[0])
        alu_entry = ALU[alu_operation](alu_src1, alu_src2)
        # check if there is a multu inst
        multiplication = ((ALUOp[0] == 2) and (funct[0] == 0b011001))
        if multiplication:
            LO_REG = alu_entry[1]
            alu_result = 0
        else:
            alu_result = alu_entry[1]

        # Branch Target
        branch_target = PC_plus_4 + (ID_EX[0][2]*4)

        if (alu_result == 0):
            Zero = 1
        else:
            Zero = 0

        if(alu_result > 0):
            greaterThanZero = 1
        else:
            greaterThanZero = 0

        # Next_PC
        # pc_mux1
        if Branch[0] == 1:
            pc_mux1 = BranchAddress[op[0]](Zero, greaterThanZero)
        else:
            pc_mux1 = PC_plus_4

        if pc_mux1 != PC_plus_4:  # i.e branch Taken
            print("Branch Taken")
        else:
            print("Branch Not Taken")

        Jump = 0
        if (Jump):
            pc_mux2 = 0
        else:
            pc_mux2 = pc_mux1

        PC = pc_mux2  # Next Instruction

        # Latch results of that stage into its pipeline reg
        EX_MEM[1] = [alu_result, readData2]
        print('Executed = ' + str(inst_assembly[2]), '\n')
        print("EX/MEM | for current MEM= ",
              EX_MEM[0], " result of current execute = ", EX_MEM[1])
        print("Zero = " + str(Zero) + "  Next_PC = " +
              str(PC) + "  Branch = "+str(Branch[0]))
        print("PC_MUX1= " + str(pc_mux1) + " PC+4 = " +
              str(PC_plus_4) + "  Branch_Target=", branch_target)

    ### MEM STAGE ###
    if (clock >= 4):
        memory_read_data = 0
        # data memory operations
        if MemWrite[1]:  # current Control
            data_mem[EX_MEM[0][0] >> 2] = EX_MEM[0][1]

        if MemRead[0]:
            memory_read_data = data_mem[EX_MEM[0][0] >> 2]

        # Latch results of that stage into its pipeline reg
        MEM_WB[1] = [memory_read_data, EX_MEM[0][0]]  # alu result
        print("Memory = " + str(inst_assembly[1]), '\n')
        print("MEM/WB | for current WB= ",
              MEM_WB[0], " result of current MEM load = ", MEM_WB[1])

    ### Write Back STAGE ###

    if (clock >= 5):
        print("WriteBack = "+str(inst_assembly[0]), '\n')
        display_regs()

    ## UPDATE PIPELINE REGISTERS ##
    # update pipeline registers

    rd[0] = rd[1]
    rd[1] = rd[2]
    rt[0] = rt[1]
    rt[1] = rt[2]
    rs[0] = rs[1]
    rs[1] = rs[2]

    EX_MEM[0] = EX_MEM[1]
    MEM_WB[0] = MEM_WB[1]

    # update Memory control lists
    MemWrite[0] = MemWrite[1]
    MemWrite[1] = MemWrite[2]
    MemRead[0] = MemRead[1]

    # update WB control lists
    MemToReg[0] = MemToReg[1]
    MemToReg[1] = MemToReg[2]
    RegWrite[0] = RegWrite[1]
    RegWrite[1] = RegWrite[2]
    RegDst[0] = RegDst[1]
    RegDst[1] = RegDst[2]

    inst_assembly[0] = inst_assembly[1]
    inst_assembly[1] = inst_assembly[2]

    IF_ID[0] = IF_ID[1]
    ID_EX[0] = ID_EX[1]
    rd[2] = rd[3]
    rt[2] = rt[3]
    rs[2] = rs[3]

    # update next instruction to current instruction for next clock cycle
    ALUSrc[0] = ALUSrc[1]
    ALUOp[0] = ALUOp[1]
    MemWrite[2] = MemWrite[3]
    MemRead[1] = MemRead[2]
    RegWrite[2] = RegWrite[3]
    RegDst[2] = RegDst[3]
    MemToReg[2] = MemToReg[3]

    funct[0] = funct[1]
    shamt[0] = shamt[1]
    op[0] = op[1]
    Branch[0] = Branch[1]

    inst_assembly[2] = inst_assembly[3]
    inst_assembly[3] = inst_assembly[4]

# -- End of for Loop
