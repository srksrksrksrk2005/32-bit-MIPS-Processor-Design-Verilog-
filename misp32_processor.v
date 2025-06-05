// instruction set
// R0 ALWAYS HAS VALUE 0
// LW R2,124(R3) - R2=MEM[R3+124]
// SW R5,-10(R3) - MEM[R3-10]=R5
// ADDI R1,R2,150 - R1=R2+150
// SUBI R1,R2,150 - R1=R2-150
// SLTI R1,R2,150 - IF R2<150 R1=1 ELSE R1=0
// BEQZ R1,Loop - branch to loop if R1=0
// BNEQZ R1,Loop - branch to loop if R1!=0

// ADD R1,R2,R3 - R1=R2+R3 
// ADD R1,R2,R0 - R1=R2  
// SUB R1,R2,R3 - R1=R2-R3
// AND R1,R2,R3 - R1=R2&R3
// OR R1,R2,R3 - R1=R2|R3
// MUL R1,R2,R3 - R1=R2*R3
// SLT R1,R2,R3 - IF R2<R3 R1=1 ELSE R1=0
// HLT - halt the processor

// J Loop - jump to loop

// R-type {6,5,5,5} ADD-000000 SUB-000001 AND-000010 OR-000011 SLT-000100 MUL-000101 HLT-111111 
// I-type {6,5,5,16} LW-001000 SW-001001 ADDI-001010 SUBI-001011 SLTI-001100 BNEQZ-001101 BEQZ-001110
// J-type {6,26} J-010000

// stages 
// IF-fetch statement - instruction pointed by PC is fetched from memory increment PC and store
// ID-instruction decode / register fecth
// EX-execution/effective Address calculation
// MME-memory access/branch completion
// WB-register write back

// hazards - data hazard, control hazard, structural hazard
// data hazard - when instruction depends on the result of previous instruction
// control hazard - when instruction depends on the result of previous instruction
// structural hazard - when two instructions want to use same resource at same time
// by using dummy instructions we can avoid data hazard
// by using branch prediction we can avoid control hazard
// by using pipeline we can avoid structural hazard
module mips32(input clk1,input clk2);
    reg [31:0] PC , IF_ID_IR , IF_ID_NPC ;
    reg [31:0] ID_EX_IR , ID_EX_NPC , ID_EX_A , ID_EX_B , ID_EX_Imm ;
    reg [2:0] MEM_WB_TYPE,EX_MEM_TYPE,ID_EX_TYPE;
    reg [31:0] EX_MEM_IR , EX_MEM_ALUout , EX_MEM_B;
    reg EX_MEM_COND;
    reg [31:0] MEM_WB_IR , MEM_WB_ALUout , MEM_WB_LMD;
    reg [31:0] regbank [0:31];
    reg [31:0] mem[0:1023];
    reg zero,sign;
    integer i;
    parameter ADD=6'b000000,SUB=6'b000001,AND=6'b000010,OR=6'b000011,SLT=6'b000100,MUL=6'b000101, XOR=6'b000110 ,
                HLT=6'b111111,
                LW=6'b001000,
                SW=6'b001001,
                ADDI=6'b001010,SUBI=6'b001011,SLTI=6'b001100,
                BNEQZ=6'b001101,BEQZ=6'b001110;
    parameter RR_ALU=3'b000,RI_ALU=3'b001,LOAD=3'b010,STORE=3'b011,BRANCH=3'b100,HALT=3'b101;
    reg HALTED; // set after HLT instructio  is completed in WB
    reg TAKEN_BRANCH; // required to disable instruction after branch;

    initial begin
        HALTED<=0;
        TAKEN_BRANCH<=0;
        EX_MEM_COND<=0;
        PC<=0;
    end
    
    //IF stage
    always@(posedge clk1)begin
        if(~HALTED)begin // halted is not set 
            if(((EX_MEM_IR[31:26]==BEQZ)&&(EX_MEM_COND))||((EX_MEM_IR[31:26]==BNEQZ)&&(EX_MEM_COND==0)))begin // checking branch is arise
                IF_ID_IR <= #2 mem[EX_MEM_ALUout]; // EX_MEM_ALUout is address calculated by ALU
                TAKEN_BRANCH <= #2 1'b1;
                IF_ID_NPC <= #2 EX_MEM_ALUout+1;
                PC <= #2 EX_MEM_ALUout+1;
            end
            else begin
                IF_ID_IR <= #2 mem[PC];
                IF_ID_NPC <= #2 PC+1;
                PC <= #2 PC+1;
            end
        end
    end
    // ID stage
    always@(posedge clk2)begin
        if(~HALTED)begin // halted is not set 
            if(IF_ID_IR[25:21]==5'b00000) ID_EX_A <= #2 0 ;
            else ID_EX_A <= #2 regbank[IF_ID_IR[25:21]] ;

            if(IF_ID_IR[20:16]==5'b00000) ID_EX_B <= #2 0 ;
            else ID_EX_B <= #2 regbank[IF_ID_IR[20:16]] ;

            ID_EX_IR <= #2 IF_ID_IR ; 
            ID_EX_NPC <= #2 IF_ID_NPC ;
            ID_EX_Imm <= #2 {{16{IF_ID_IR[15]}},{IF_ID_IR[15:0]}} ; // sign extension of last 16 bits offset/imediate

            case(IF_ID_IR[31:26])
                ADD,SUB,AND,OR,SLT,MUL,XOR : ID_EX_TYPE <= #2 RR_ALU ; // register type
                ADDI,SUBI,SLTI             : ID_EX_TYPE <= #2 RI_ALU ; // register and immediate type
                LW                         : ID_EX_TYPE <= #2 LOAD ; // loding
                SW                         : ID_EX_TYPE <= #2 STORE ; // storing
                BEQZ,BNEQZ                 : ID_EX_TYPE <= #2 BRANCH ; // branch
                default                    : ID_EX_TYPE <= #2 HALT ; // other than above consider as HALT
            endcase
        end
    end

    // EX stage
    always@(posedge clk1)begin
        if(~HALTED)begin
            EX_MEM_TYPE <= #2 ID_EX_TYPE ;
            EX_MEM_IR <= #2 ID_EX_IR ;
            TAKEN_BRANCH <= #2 0;
            case(ID_EX_TYPE)
                RR_ALU: begin 
                    case(ID_EX_IR[31:26])
                        ADD : EX_MEM_ALUout <= #2 ID_EX_A + ID_EX_B ;
                        SUB : EX_MEM_ALUout <= #2 ID_EX_A - ID_EX_B ;
                        AND : EX_MEM_ALUout <= #2 ID_EX_A & ID_EX_B ;
                        OR  : EX_MEM_ALUout <= #2 ID_EX_A | ID_EX_B ;
                        SLT : EX_MEM_ALUout <= #2 ID_EX_A < ID_EX_B ;
                        MUL : EX_MEM_ALUout <= #2 ID_EX_A * ID_EX_B ;
                        XOR : EX_MEM_ALUout <= #2 ID_EX_A ^ ID_EX_B ;
                        default : EX_MEM_ALUout <= #2 32'hxxxxxxxx;
                    endcase
                end
                RI_ALU:begin
                    case(ID_EX_IR[31:26])
                        ADDI : EX_MEM_ALUout <= #2 ID_EX_A + ID_EX_Imm ;
                        SUBI : EX_MEM_ALUout <= #2 ID_EX_A - ID_EX_Imm ;
                        SLTI : EX_MEM_ALUout <= #2 ID_EX_A < ID_EX_Imm ;
                        default : EX_MEM_ALUout <= #2 32'hxxxxxxxx;
                    endcase
                end
                LOAD,STORE:begin
                    EX_MEM_ALUout <= #2 ID_EX_A + ID_EX_Imm ; // RESULTING ADDRESS
                    EX_MEM_B <= #2 ID_EX_B;
                end
                BRANCH:begin
                    EX_MEM_ALUout <= #2 ID_EX_NPC + ID_EX_Imm ; // RESULTING ADDRESS
                    EX_MEM_COND <= #2 (ID_EX_A==0);
                end
            endcase
        end
    end

    // mem STAGE
    always@(posedge clk2)begin
        if(~HALTED)begin // halted is not set 
            MEM_WB_TYPE <= #2 EX_MEM_TYPE;
            MEM_WB_IR <= #2 EX_MEM_IR ;

            case(EX_MEM_TYPE)
                RR_ALU,RI_ALU :begin 
                    MEM_WB_ALUout <= #2 EX_MEM_ALUout ;
                end
                LOAD          : MEM_WB_LMD <= #2 mem[EX_MEM_ALUout] ;
                STORE         : if(TAKEN_BRANCH == 0) mem[EX_MEM_ALUout] <= #2 EX_MEM_B ; // BRANCHING IS GOING DIABLE WRITE
            endcase
        end
    end
    // WB STAGE
    always@(posedge clk1)begin
        if(TAKEN_BRANCH == 0)begin // BRANCHING IS GOING DIABLE WRITE
            case(MEM_WB_TYPE)
                RR_ALU : regbank[MEM_WB_IR[15:11]] <= #2 MEM_WB_ALUout ; // RD
                RI_ALU : regbank[MEM_WB_IR[20:16]] <= #2 MEM_WB_ALUout ;
                LOAD   : regbank[MEM_WB_IR[20:16]] <= #2 MEM_WB_LMD ;
                HALT   : HALTED <= #2 1'b1 ;
            endcase
        end
    end
endmodule

module test_mips32;
    reg clk1,clk2;
    integer k;
    mips32 MIPS(clk1,clk2);
    initial begin
        clk1=0;clk2=0;
        forever begin
            #5 clk1=1;
            #5 clk1=0;
            #5 clk2=1;
            #5 clk2=0;
        end
    end
    //1)ADDING NUMBERS opcode src   dest     immediate
    // ADDI R1,R0,-15 - 001010 00000 00001 1111111111110001
    // ADDI R2,R0,-10 - 001010 00000 00010 0000000000010100
    // ADDI R3,R0,25 - 001010 00000 00011 0000000000011001
    //                 opcode  src1  src2  dest
    // ADD R4,R1,R2 -  000000 00001 00010 00100 00000 000000
    // ADD R5,R4,R3 -  000000 00100 00011 00101 00000 000000
    // HLT          -  111111 00000 00000 00101 00000 000000
    // initial begin
    //     for(k=0;k<32;k=k+1)begin
    //         MIPS.regbank[k]<=k;
    //     end
    //     MIPS.mem[0] <= 32'b00101000000000010000000000001010;
    //     MIPS.mem[1] <= 32'b00101000000000100000000000010100;
    //     MIPS.mem[2] <= 32'b00101000000000110000000000011001;

    //     MIPS.mem[3] <= 32'h0ce77800; // OR R7,r7,r7 -- dummy instruction to aviod data hazard
    //     MIPS.mem[4] <= 32'h0ce77800; // OR R7,r7,r7 -- dummy instruction to aviod data hazard
    //     // as we reuqire R1,R2 for next instruction to execute 

    //     MIPS.mem[5] <= 32'b00000000001000100010000000000000;

    //     MIPS.mem[6] <= 32'h0ce77800; // OR R7,r7,r7 -- dummy instruction to aviod data hazard

    //     MIPS.mem[7] <= 32'b00000000100000110010100000000000;
    //     MIPS.mem[8] <= 32'b11111100000000000010100000000000;
    //     #190 $display(" R1=%d R2=%d R3=%d R4=%d R5=%d",MIPS.regbank[1],MIPS.regbank[2],MIPS.regbank[3],MIPS.regbank[4],MIPS.regbank[5]);
    // end

    //2)LOADING AND STORING NUMBERS
    // ADDI R1,R0,120 - 001010 00000 00001 0000000001111000
    // LW R2,0(R1) - 001000 00001 00010 0000000000000000
    // ADDI R2,R2,45 - 001010 00010 00010 0000000000101101
    // SW R2,1(R1) - 001001 00010 00001 0000000000000001
    // HLT          - 111111 00000 00000 00000 00000 000000
    // initial begin
    //     for(k=0;k<32;k=k+1)begin
    //         MIPS.regbank[k]<=0;
    //     end
    //     MIPS.mem[0] <= 32'h28010078;
    //     MIPS.mem[1] <= 32'h0c631800; // OR R7,r7,r7 -- dummy instruction to aviod data hazard
    //     MIPS.mem[2] <= 32'h20220000;
    //     MIPS.mem[3] <= 32'h0c631800; // OR R7,r7,r7 -- dummy instruction to aviod data hazard
    //     MIPS.mem[4] <= 32'h2842002d;
    //     MIPS.mem[5] <= 32'h0c631800; // OR R7,r7,r7 -- dummy instruction to aviod data hazard
    //     MIPS.mem[6] <= 32'h24220001;
    //     MIPS.mem[7] <= 32'hfc000000;
    //     MIPS.mem[120] <= 85;
    //     #280 $display(" R1=%d , R2=%d , MEM[120]=%d , MEM[121]=%d",MIPS.regbank[1],MIPS.regbank[2],MIPS.mem[120],MIPS.mem[121]);
    // end

    // 3) compute factorial of N which is stored in memory locartion 200 and result stored in 198 memory location
    // ADDI R10,R0,200 - 001010 00000 00001 0000000011001000
    // ADDI R2,R0,1 - 001010 00000 00010 0000000000000001
    // LW R3,0(R10) - 001000 01010 00011 0000000000000000
    // LOOP: MUL R2,R2,R3 - 000101 00010 00010 00011 00000 000000
    // SUBI R3,R3,1 - 001011 00011 00011 0000000000000001
    // BNEQZ R3,LOOP - 001101 00011 00000 1111111111111101 substract 3 from Next PC to jump to loop
    // SW R2,-2(R10) - 001001 00010 00001 1111111111111110
    // HLT          - 111111 00000 00000 00000 00000 000000
    initial begin
        for(k=0;k<32;k=k+1)begin
            MIPS.regbank[k]<=0;
        end
        MIPS.mem[0] <= 32'h280a00c8;
        MIPS.mem[1] <= 32'h28020001;
        MIPS.mem[2] <= 32'h0e94a000; // OR R20,r20,r20 -- dummy instruction to aviod data hazard
        MIPS.mem[3] <= 32'h21430000;
        MIPS.mem[4] <= 32'h0e94a000; // OR R7,r7,r7 -- dummy instruction to aviod data hazard
        MIPS.mem[5] <= 32'h14431000;
        MIPS.mem[6] <= 32'h2c630001;
        MIPS.mem[7] <= 32'h0e94a000; // OR R7,r7,r7 -- dummy instruction to aviod data hazard
        MIPS.mem[8] <= 32'h3460fffc;
        MIPS.mem[9] <= 32'h2542fffe;
        MIPS.mem[10] <= 32'hfc000000;
        MIPS.mem[200] <= 6;
        $display("6 factorial :");
        #2000 $display(" R2=%d , MEM[200]=%d , MEM[198]=%d",MIPS.regbank[2],MIPS.mem[200],MIPS.mem[198]);
    end



    initial begin
        $dumpfile("test_MIPS32.vcd");
        $dumpvars(0, test_mips32);
        $monitor("R2=%4d",MIPS.regbank[2]);
        #3000 $finish;
    end

endmodule