// c4.c - C in four functions

// char, int, and pointer types
// if, while, return, and expression statements
// just enough features to allow self-compilation and a bit more

// Written by Robert Swierczek

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <fcntl.h>
#define int long long

char *p, *lp, // current position in source code
     *data;   // data/bss pointer

int *e, *le,  // current position in emitted code
    *id,      // currently parsed identifier
    *sym,     // symbol table (simple list of identifiers)
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line,     // current line number
    src,      // print source and assembly flag
    debug;    // print executed instructions

// tokens and classes (operators last and in precedence order)
enum {
  Num = 128, Fun, Sys, Glo, Loc, Id,
  Char, Else, Enum, If, Int, Return, Sizeof, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

// opcodes
enum { LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
       OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT };

// types
enum { CHAR, INT, PTR };

// identifier offsets (since we can't create an ident struct)
// Symbol table entry's field indexes, except for `Idsz`.
// `Hash`: Symbol name's hash value.
// `Name`: Symbol name's string address.
// `Class`: Symbol type:
// - Num: Enum name.
// - Fun: Function name.
// - Sys: System call name.
// - Glo: Global variable name.
// - Loc: Local variable name.
// `Type`: Associated value type. e.g. `CHAR`, `INT`.
// `Val`: Associated value.
// `HClass`: Backup field for `Class` field.
// `HType`: Backup field for `Type` field.
// `HVal`: Backup field for `Val` field.
// `Idsz`: Symbol table entry size.
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };

// Read token.
void next()
{
  char *pp;

  // Get current character.
  // While current character is not `\0`.
  // The source code has been read into source code buffer and ended with `\0`.
  while (tk = *p) {
    // Point to next character.
    ++p;

    // If current character is newline.
    if (tk == '\n') {
      // If switch for printing source code line and corresponding instructions
      // is on.
      if (src) {
        // Print source code line.
        printf("%d: %.*s", line, p - lp, lp);

        // Point `lp` to the last newline.
        lp = p;

        // While have instruction to print.
        while (le < e) {
          // Print opcode.
          printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                           "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                           "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[*++le * 5]);

          // If the opcode <= ADJ, it has operand.
          // Print operand.
          if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n");
        }
      }

      // Increment line number.
      ++line;
    }
    // If current character is `#`, it is preprocessing directive.
    // Preprocessing directive is ignored.
    else if (tk == '#') {
      // While current character is not `\0` and current character is not
      // newline.
      // Skip current character.
      while (*p != 0 && *p != '\n') ++p;
    }
    // If current character is letter or underscore, it is identifier.
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {
      // Point `pp` to the first character.
      pp = p - 1;

      // While current character is letter, digit, or underscore.
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
        // Use current character to compute hash value.
        tk = tk * 147 + *p++;

      // Combine the hash value with string length.
      tk = (tk << 6) + (p - pp);

      // Point `id` to symbol table.
      id = sym;

      // While current symbol table entry is in use.
      while (id[Tk]) {
        // If current symbol table entry's hash is equal and name is equal, it
        // means the name has been seen before.
        // Set token type be the entry's token type.
        if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { tk = id[Tk]; return; }

        // Point to next table entry.
        id = id + Idsz;
      }

      // At this point, existing symbol name is not found.
      // `id` is pointing to the first unused symbol table entry.

      // Store the name's string address.
      id[Name] = (int)pp;

      // Store the name's hash value.
      id[Hash] = tk;

      // Set token type.
      tk = id[Tk] = Id;

      return;
    }
    // If current character is digit, it is number constant.
    else if (tk >= '0' && tk <= '9') {
      // If current character is not `0`, it is decimal notation.
      // Convert decimal notation to value.
      if (ival = tk - '0') { while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; }
      // If current character is `0` and following character is `x` or
      // `X`, it is hexadecimal notation.
      else if (*p == 'x' || *p == 'X') {
        // Convert hexadecimal notation to value.
        while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
          ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
      }
      // If current character is `0` and following character is not `x` or
      // `X`, it is octal notation.
      // Convert octal notation to value.
      else { while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0'; }

      // Set token type.
      tk = Num;

      return;
    }
    // If current character is `/`, it is comments or division operator.
    else if (tk == '/') {
      // If following character is `/`, it is comments.
      if (*p == '/') {
        // Point to next character.
        ++p;

        // While current character is not `\0` and current character is not
        // newline.
        // Skip current character.
        while (*p != 0 && *p != '\n') ++p;
      }
      // If following character is not `/`, it is division operator.
      else {
        // Set token type.
        tk = Div;

        return;
      }
    }
    // If current character is `'` or `"`, it is character constant or string
    // constant.
    else if (tk == '\'' || tk == '"') {
      // Store data buffer's current location.
      pp = data;

      // While current character is not `\0` and current character is not the
      // quote character.
      while (*p != 0 && *p != tk) {
        // If current character is `\`, it is escape notation or simply `\`
        // character.
        if ((ival = *p++) == '\\') {
          // If following character is `n`, it is newline escape,
          if ((ival = *p++) == 'n') ival = '\n';
        }

        // If it is string constant, copy current character to data buffer.
        if (tk == '"') *data++ = ival;
      }

      // Point to next character.
      ++p;

      // If it is string constant, use the string's address as the token's
      // associated value. The token type is `"`.
      // If it is character constant, use the character's value as the token's
      // associated value. Set token type be number constant.
      if (tk == '"') ival = (int)pp; else tk = Num;

      return;
    }
    else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; }
    else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; }
    else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; }
    else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; }
    else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; }
    else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; }
    else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; }
    else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; }
    else if (tk == '^') { tk = Xor; return; }
    else if (tk == '%') { tk = Mod; return; }
    else if (tk == '*') { tk = Mul; return; }
    else if (tk == '[') { tk = Brak; return; }
    else if (tk == '?') { tk = Cond; return; }
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return;
  }
}

// Parse expression.
// `lev`: Current operator precedence. Greater value means higher precedence.
// Operator precedence (lower first):
// Assign  =
// Cond    ?
// Lor     ||
// Lan     &&
// Or      |
// Xor     ^
// And     &
// Eq      ==
// Ne      !=
// Lt      <
// Gt      >
// Le      <=
// Ge      >=
// Shl     <<
// Shr     >>
// Add     +
// Sub     -
// Mul     *
// Div     /
// Mod     %
// Inc     ++
// Dec     --
// Brak    [
void expr(int lev)
{
  int t, *d;

  // If current token is input end, print error and exit program.
  if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); }

  // If current token is number constant.
  // Add `IMM` instruction to load the number's value to register.
  // Set result value type be `INT`.
  else if (tk == Num) { *++e = IMM; *++e = ival; next(); ty = INT; }
  // If current token is string constant.
  else if (tk == '"') {
    // Add `IMM` instruction to load the string's address to register.
    // Read token.
    *++e = IMM; *++e = ival; next();

    // While current token is string constant, it is adjacent string
    // constants, e.g. "abc" "def".
    // In `next`, the string's characters have been copied to data buffer.
    // This implements concatenation of adjacent string constants.
    // Read token.
    while (tk == '"') next();

    // Point `data` to next int-aligned address.
    // E.g. `-sizeof(int)` is -4, i.e. 0b11111100.
    // This guarantees to leave at least one '\0' after the string.
    //
    // Set result value type be char pointer.
    // CHAR + PTR = PTR because CHAR is 0.
    data = (char *)((int)data + sizeof(int) & -sizeof(int)); ty = PTR;
  }
  // If current token is `sizeof` operator.
  else if (tk == Sizeof) {
    // Read token.
    // If current token is `(`, read token, else print error and exit
    // program.
    next(); if (tk == '(') next(); else { printf("%d: open paren expected in sizeof\n", line); exit(-1); }

    // Set operand value type be `INT`.
    // If current token is `int`, read token.
    // If current token is `char`, read token, set operand value type be
    // `CHAR`.
    ty = INT; if (tk == Int) next(); else if (tk == Char) { next(); ty = CHAR; }

    // While current token is `*`, it is pointer type.
    // Add `PTR` to the operand value type.
    while (tk == Mul) { next(); ty = ty + PTR; }

    // If current token is `)`, read token, else print error and exit program.
    if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); }

    // Add `IMM` instruction to load the operand value's size to register.
    *++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);

    // Set result value type be `INT`.
    ty = INT;
  }
  // If current token is identifier.
  else if (tk == Id) {
    // Store the identifier's symbol table entry address.
    // Read token.
    d = id; next();

    // If current token is `(`, it is function call.
    if (tk == '(') {
      // Read token.
      next();

      // Arguments count.
      t = 0;

      // While current token is not `)`.
      // Parse argument expression.
      // Add `PSH` instruction to push the argument to stack.
      // Increment arguments count.
      // If current token is `,`, skip.
      while (tk != ')') { expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); }

      // Skip `)`
      next();

      // If it is system call,
      // add the system call's opcode to instruction buffer.
      if (d[Class] == Sys) *++e = d[Val];
      // If it is function call,
      // add `JSR` opcode and the function address to instruction buffer.
      else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; }
      // Else print error message and exit program.
      else { printf("%d: bad function call\n", line); exit(-1); }

      // If have arguments.
      // Add `ADJ` instruction and arguments count to instruction buffer to
      // pop arguments off stack after returning from function call.
      if (t) { *++e = ADJ; *++e = t; }

      // Set result value type be the system call or function's return type.
      ty = d[Type];
    }
    // If it is enum name.
    // Add `IMM` instruction to load the enum value to register.
    // Set result value type be `INT`.
    else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; }
    // If it is none of above, assume it is a variable name.
    else {
      // 6S71X
      // If it is local variable, add `LEA` opcode and the local variable's
      // offset to instruction buffer to load the local variable's address to
      // register.
      if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; }
      // If it is global variable, add `IMM` instruction to load the global
      // variable's address to register.
      else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; }
      // Else print error message and exit program.
      else { printf("%d: undefined variable\n", line); exit(-1); }

      // 2WQE9
      // Add `LC`/`LI` instruction to load the value on the address in register
      // to register.
      *++e = ((ty = d[Type]) == CHAR) ? LC : LI;
    }
  }
  // If current token is `(`, it is cast or expression in parentheses.
  else if (tk == '(') {
    // Read token.
    next();

    // If current token is `int` or `char`, it is cast.
    if (tk == Int || tk == Char) {
      // Get the cast's base data type.
      // Read token.
      t = (tk == Int) ? INT : CHAR; next();

      // While current token is `*`, it is pointer type.
      // Add `PTR` to the cast's data type.
      while (tk == Mul) { next(); t = t + PTR; }

      // If current token is not `)`, print error and exit program.
      if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }

      // Parse casted expression.
      // Use `Inc` to allow only `++`, `--`, `[]` operators in the expression.
      expr(Inc);

      // Set result value type be the cast's data type.
      ty = t;
    }
    // If current token is not `int` or `char`, it is expression in
    // parentheses.
    else {
      // Parse expression.
      expr(Assign);

      // If current token is not `)`, print error and exit program.
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    }
  }
  // If current token is `*`, it is dereference operator.
  else if (tk == Mul) {
    // Read token.
    // Parse operand expression.
    // Use `Inc` to allow only `++`, `--`, `[]` operators in the expression.
    next(); expr(Inc);

    // If operand value type is not pointer, print error and exit program.
    if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); }

    // Add `LC`/`LI` instruction to load the value on the address in register
    // to register.
    *++e = (ty == CHAR) ? LC : LI;
  }
  // If current token is `&`, it is address-of operator.
  else if (tk == And) {
    // Read token.
    // Parse operand expression.
    // Use `Inc` to allow only `++`, `--`, `[]` operators in the expression.
    next(); expr(Inc);

    // The operand of the address-of operator should be a variable.
    // The instructions to get the variable's address has been added at 6S71X.
    // Only need to remove the `LC`/`LI` instruction added at 2WQE9.
    // If current instruction is `LC`/`LI`, remove it, else print error and
    // exit program.
    if (*e == LC || *e == LI) --e; else { printf("%d: bad address-of\n", line); exit(-1); }

    // Set result value type be pointer to current value type.
    ty = ty + PTR;
  }
  // If current token is `!`, it is boolean negation operator.
  // Add instructions to compute `x == 0` because `!x` is equivalent to
  // `x == 0`.
  // Set result value type be `INT`.
  else if (tk == '!') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; }
  // If current token is `~`, it is bitwise inversion operator.
  // Add instructions to compute `x ^ -1` because `~x` is equivalent to
  // `x ^ -1`.
  // Set result value type be `INT`.
  else if (tk == '~') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; }
  // If current token is `+`, it is unary addition operator.
  // Read token.
  // Parse operand expression.
  // Set result value type be `INT`.
  else if (tk == Add) { next(); expr(Inc); ty = INT; }
  // If current token is `-`, it is unary subtraction operator.
  else if (tk == Sub) {
    // Read token.
    // Add `IMM` instruction to load number constant's negated value or `-1`
    // to register.
    next(); *++e = IMM;

    // If operand is number constant, add negated value to instruction buffer.
    // If operand is not number constant, add `-1` to instruction buffer. Add
    // `PSH` instruction to push `-1` in register to stack. Parse operand
    // expression. Add `MUL` instruction to multiply `-1` on stack by the
    // operand value in register.
    if (tk == Num) { *++e = -ival; next(); } else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }

    // Set result value type be `INT`.
    ty = INT;
  }
  // If current token is prefix increment or decrement operator.
  else if (tk == Inc || tk == Dec) {
    // Store current token type.
    // Read token.
    // Parse operand expression.
    t = tk; next(); expr(Inc);

    // If current instruction is `LC`, insert a `PSH` instruction before `LC`
    // to push variable address in register to stack for use by the `SC`
    // instruction added below.
    if (*e == LC) { *e = PSH; *++e = LC; }
    // If current instruction is `LI`, insert a `PSH` instruction before `LI`
    // to push variable address in register to stack for use by the `SI`
    // instruction added below.
    else if (*e == LI) { *e = PSH; *++e = LI; }
    // Else print error and exit program.
    else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }

    // Add `PSH` instruction to push operand value in register to stack
    // for use by the `ADD`/`SUB` instruction added below.
    *++e = PSH;

    // Add `IMM` instruction to load increment/decrement value to register.
    *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);

    // Add `ADD`/`SUB` instruction to compute result value.
    *++e = (t == Inc) ? ADD : SUB;

    // Add `SC`/`SI` instruction to save result value in register to address
    // held on stack.
    *++e = (ty == CHAR) ? SC : SI;
  }
  // Else print error and exit program.
  else { printf("%d: bad expression\n", line); exit(-1); }

  // While current token type is >= current operator precedence,
  // it is an operator that should be handled here.
  while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method
    // Store current value type.
    t = ty;

    // If current token is assignment operator.
    if (tk == Assign) {
      // Read token.
      next();

      // If current instruction is `LC`/`LI`, current value in register is
      // variable address, replace current instruction with `PSH` instruction
      // to push the variable address to stack for use by the `SC`/`SI`
      // instruction added below.
      // If current instruction is not `LC`/`LI`, current value in register is
      // not variable address, print error and exit program.
      if (*e == LC || *e == LI) *e = PSH; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }

      // Parse RHS expression.
      // Add `SC`/`SI` instruction to save value in register to variable
      // address held on stack.
      expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
    }
    // If current token is conditional operator.
    else if (tk == Cond) {
      // Read token.
      next();

      // Add jump-if-zero instruction `BZ` to jump to false branch.
      // Point `d` to the jump address field to be patched later.
      *++e = BZ; d = ++e;

      // Parse true branch's expression.
      expr(Assign);

      // If current token is not `:`, print error and exit program.
      if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }

      // Patch the jump address field pointed to by `d` to hold the address of
      // false branch.
      // `+ 3` counts the `JMP` instruction added below.
      //
      // Add `JMP` instruction after the true branch to jump over the false
      // branch.
      // Point `d` to the jump address field to be patched later.
      *d = (int)(e + 3); *++e = JMP; d = ++e;

      // Parse false branch's expression.
      expr(Cond);

      // Patch the jump address field pointed to by `d` to hold the address
      // past the false branch.
      *d = (int)(e + 1);
    }
    // If current token is logical OR operator.
    // Read token.
    // Add jump-if-nonzero instruction `BNZ` to implement short circuit.
    // Point `d` to the jump address field to be patched later.
    // Parse RHS expression.
    // Patch the jump address field pointed to by `d` to hold the address past
    // the RHS expression.
    // Set result value type be `INT`.
    else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (int)(e + 1); ty = INT; }
    // If current token is logical AND operator.
    // Read token.
    // Add jump-if-zero instruction `BZ` to implement short circuit.
    // Point `d` to the jump address field to be patched later.
    // Parse RHS expression.
    // Patch the jump address field pointed to by `d` to hold the address past
    // the RHS expression.
    // Set result value type be `INT`.
    else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (int)(e + 1); ty = INT; }
    // If current token is bitwise OR operator.
    // Read token.
    // Add `PSH` instruction to push LHS value in register to stack.
    // Parse RHS expression.
    // Add `OR` instruction to compute the result.
    // Set result value type be `INT`.
    // The following operators are similar.
    else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; }
    else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; }
    else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; }
    else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; }
    else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; }
    else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; }
    else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; }
    else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; }
    else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; }
    else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; }
    else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; }
    // If current token is addition operator.
    else if (tk == Add) {
      // Read token.
      // Add `PSH` instruction to push LHS value in register to stack.
      // Parse RHS expression.
      next(); *++e = PSH; expr(Mul);

      // If LHS value type is pointer,
      // the RHS value should be multiplied by int size to get address offset.
      // Add `PSH` instruction to push RHS value in register to stack.
      // Add `IMM` instruction to load int size to register.
      // Add `MUL` instruction to multiply RHS value on stack by int size in
      // register to get the address offset.
      if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }

      // Add addition instruction to add LHS value on stack to RHS value in
      // register.
      *++e = ADD;
    }
    // If current token is subtraction operator.
    else if (tk == Sub) {
      // Read token.
      // Add `PSH` instruction to push LHS value in register to stack.
      // Parse RHS expression.
      next(); *++e = PSH; expr(Mul);
      // If LHS value type is pointer and RHS value type is pointer,
      // the subtraction result should be divided by int size to get int-size
      // difference.
      // Add `SUB` instruction to subtract LHS value on stack by RHS value in
      // register.
      // Add `PSH` instruction to push the address difference in register to
      // stack.
      // Add `IMM` instruction to load int size to register.
      // Add `DIV` instruction to divide the address difference on stack by the
      // int size in register to get int-size difference.
      if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV; ty = INT; }
      // If LHS value type is pointer and RHS value type is not pointer,
      // the RHS value should be multiplied by int size to get address offset.
      // Add `PSH` instruction to push LHS value in register to stack.
      // Add `IMM` instruction to load int size to register.
      // Add `MUL` instruction to multiply RHS value on stack by int size in
      // register to get the address offset.
      // Add 'SUB' instruction to subtract LHS value on stack by the address
      // offset in register.
      else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB; }
      // If LHS value type is not pointer.
      // Add `SUB` instruction to subtract LHS value on stack by RHS value in
      // register.
      else *++e = SUB;
    }
    // If current token is multiplication operator.
    // Add `PSH` instruction to push LHS value in register to stack.
    // Parse RHS expression.
    // Add `MUL` instruction to multiply LHS value on stack by RHS value in
    // register.
    // Set result value type be `INT`.
    // The following operators are similar.
    else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }
    else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }
    else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }
    // If current token is postfix increment or decrement operator.
    else if (tk == Inc || tk == Dec) {
      // If current instruction is `LC`, insert a `PSH` instruction before `LC`
      // to push variable address in register to stack for use by the `SC`
      // instruction added below.
      if (*e == LC) { *e = PSH; *++e = LC; }
      // If current instruction is `LI`, insert a `PSH` instruction before `LI`
      // to push variable address in register to stack for use by the `SI`
      // instruction added below.
      else if (*e == LI) { *e = PSH; *++e = LI; }
      // Else print error and exit program.
      else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }

      // Add `PSH` instruction to push operand value in register to stack.
      // Add `IMM` instruction to load increment/decrement size to register.
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);

      // Add `ADD`/`SUB` instruction to compute the post value.
      *++e = (tk == Inc) ? ADD : SUB;

      // Add `SC`/`SI` instruction to save the post value in register to
      // variable.
      *++e = (ty == CHAR) ? SC : SI;

      // Add `PSH` instruction to push the post value in register to stack.
      // Add `IMM` instruction to load increment/decrement size to register.
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);

      // Add `SUB`/`ADD` instruction to compute the old value.
      // This implements postfix semantics.
      *++e = (tk == Inc) ? SUB : ADD;

      // Read token.
      next();
    }
    // If current token is `[`, it is array subscript.
    else if (tk == Brak) {
      // Read token.
      // Add `PSH` instruction to push the base address in register to stack.
      // Parse subscript expression.
      next(); *++e = PSH; expr(Assign);

      // If current token is not `]`, print error and exit program.
      if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }

      // If base address's value type is int pointer or pointer to pointer,
      // the subscript value should be multiplied by int size to get address
      // offset. `t == PTR` is char pointer `char*`, which needs not doing so.
      // Add `PSH` instruction to push subscript value in register to stack.
      // Add `IMM` instruction to load int size to register.
      // Add `MUL` instruction to compute address offset.
      if (t > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
      // If base address's value type is not pointer, print error and exit
      // program.
      else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }

      // Add `ADD` instruction to add the address offset to the base address.
      *++e = ADD;

      // Add `LC`/`LI` instruction to load the value on the address in register
      // to register.
      *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
    }
    // If current token is not a known operator, print error and exit program.
    else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); }
  }
}

// Parse statement.
void stmt()
{
  int *a, *b;

  // If current token is `if`.
  if (tk == If) {
    // Read token.
    next();

    // If current token is not `(`, print error and exit program.
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }

    // Parse test expression.
    expr(Assign);

    // If current token is not `)`, print error and exit program.
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }

    // Add jump-if-zero instruction `BZ` to jump over the true branch.
    // Point `b` to the jump address field to be patched later.
    *++e = BZ; b = ++e;

    // Parse true branch's statement.
    stmt();

    // If current token is `else`.
    if (tk == Else) {
      // Patch the jump address field pointed to by `b` to hold the address of
      // else branch.
      // `e + 3` excludes the `JMP` instruction added below.
      //
      // Add `JMP` instruction after the true branch to jump over the else
      // branch.
      //
      // Point `b` to the jump address field to be patched later.
      *b = (int)(e + 3); *++e = JMP; b = ++e;

      // Read token.
      next();

      // Parse else branch's statement.
      stmt();
    }

    // Patch the jump address field pointed to by `b` to hold the address past
    // the if-else structure.
    *b = (int)(e + 1);
  }
  // If current token is `while`.
  else if (tk == While) {
    // Read token.
    next();

    // Point `a` to the loop's test expression's address.
    a = e + 1;

    // If current token is not `(`, print error and exit program.
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }

    // Parse test expression.
    expr(Assign);

    // If current token is not `)`, print error and exit program.
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }

    // Add jump-if-zero instruction `BZ` to jump over loop body.
    // Point `b` to the jump address field to be patched later.
    *++e = BZ; b = ++e;

    // Parse loop body's statement.
    stmt();

    // Add `JMP` instruction to jump to test expression.
    *++e = JMP; *++e = (int)a;

    // Patch the jump address field pointed to by `b` to hold the address past
    // the loop structure.
    *b = (int)(e + 1);
  }
  // If current token is `return`.
  else if (tk == Return) {
    // Read token.
    next();

    // If current token is not `;`, it is return expression.
    // Parse return expression.
    if (tk != ';') expr(Assign);

    // Add `LEV` instruction to leave the function.
    *++e = LEV;

    // If current token is `;`, read token, else print error and exit program.
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
  // If current token is `{`, it is block.
  else if (tk == '{') {
    // Read token.
    next();

    // While current token is not `}`.
    // Parse statement.
    while (tk != '}') stmt();

    // Read token.
    next();
  }
  // If current token is `;`, it is statement end.
  else if (tk == ';') {
    // Read token.
    next();
  }
  // If current token is none of above, assume it is expression.
  else {
    // Parse expression.
    expr(Assign);

    // If current token is `;`, read token, else print error and exit program.
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
}

int main(int argc, char **argv)
{
  int fd, bt, ty, poolsz, *idmain;
  int *pc, *sp, *bp, a, cycle; // vm registers
  int i, *t; // temps

  // Decrement `argc` to get the number of command line arguments.
  // Increment `argv` to point to the first command line argument.
  --argc; ++argv;

  // If command line argument `-s` is given,
  // turn on switch for printing source code line and corresponding
  // instructions.
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; }

  // If command line argument `-d` is given,
  // turn on debug switch.
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; }

  // If source code file path is not given, print program usage and exit
  // program.
  if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; }

  // Open source code file.
  // If failed, print error and exit program.
  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }

  // Set buffer size.
  poolsz = 256*1024; // arbitrary size

  // Allocate symbol table.
  // If failed, print error and exit program.
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }

  // Allocate instruction buffer.
  // If failed, print error and exit program.
  if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }

  // Allocate data buffer.
  // If failed, print error and exit program.
  if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }

  // Allocate stack.
  // If failed, print error and exit program.
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; }

  // Clear the buffers.
  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);

  // Keywords and system call names.
  p = "char else enum if int return sizeof while "
      "open read close printf malloc free memset memcmp exit void main";

  // For each keyword from `char` to `while`,
  // call `next` to create symbol table entry,
  // store the keyword's token type in the symbol table entry's `Tk` field.
  i = Char; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table

  // For each system call name from `open` to `exit`,
  // call `next` to create symbol table entry,
  // set the symbol table entry's symbol type field be `Sys`,
  // set the symbol table entry's associated value type field be the system
  // call's return type,
  // set the symbol table entry's associated value field be the system call's
  // opcode.
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // add library to symbol table

  // Create symbol table entry for `void`.
  next(); id[Tk] = Char; // handle void type

  // Create symbol table entry for `main`.
  // Point `idmain` to the symbol table entry.
  next(); idmain = id; // keep track of main

  // Allocate source code buffer.
  // If failed, print error and exit program.
  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }

  // Read source code from source code file into source code buffer.
  // If failed, print error and exit program.
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; }

  // Add end maker `\0` after the source code in source code buffer.
  p[i] = 0;

  // Close source code file.
  close(fd);

  // parse declarations
  line = 1;

  // Read token.
  next();

  // While current token is not input end.
  while (tk) {
    // Set result value type.
    bt = INT; // basetype

    // If current token is `int`, read token.
    if (tk == Int) next();
    // If current token is `char`, read token, set result value type be `CHAR`.
    else if (tk == Char) { next(); bt = CHAR; }
    // If current token is `enum`, it is enum definition.
    else if (tk == Enum) {
      // Read token.
      next();

      // If current token is not `{`, it means having enum type name.
      // Skip the enum type name.
      if (tk != '{') next();

      // If current token is `{`.
      if (tk == '{') {
        // Read token.
        next();

        // Enum value starts from 0.
        i = 0;

        // While current token is not `}`
        while (tk != '}') {
          // Current token should be enum name.
          // If current token is not identifier, print error and exit program.
          if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; }

          // Read token.
          next();

          // If current token is assignment operator.
          if (tk == Assign) {
            // Read token.
            next();

            // If current token is not number constant, print error and exit
            // program.
            if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; }

            // Set enum value.
            i = ival;

            // Read token.
            next();
          }

          // `id` is pointing to the enum name's symbol table entry.
          // Set the symbol table entry's symbol type be `Num`.
          // Set the symbol table entry's associated value type be `INT`.
          // Set the symbol table entry's associated value be the enum value.
          id[Class] = Num; id[Type] = INT; id[Val] = i++;

          // If current token is `,`, skip.
          if (tk == ',') next();
        }

        // Skip `}`.
        next();
      }
    }

    // While current token is not statement end or block end.
    while (tk != ';' && tk != '}') {
      // Set value type.
      ty = bt;

      // While current token is `*`, it is pointer type.
      // Read token.
      // Add `PTR` to the value type.
      while (tk == Mul) { next(); ty = ty + PTR; }

      // Current token should be variable name or function name.
      // If current token is not identifier, print error and exit program.
      if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; }

      // If the name has been defined before, print error and exit program.
      if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; }

      // Read token.
      next();

      // Store the variable's data type or the function's return type.
      id[Type] = ty;

      // If current token is `(`, it is function definition.
      if (tk == '(') { // function
        // Store symbol type.
        id[Class] = Fun;

        // Store function address.
        // `+ 1` is because the code to add instruction always uses `++e`.
        id[Val] = (int)(e + 1);

        // Read token.
        // `i` is parameter's index.
        next(); i = 0;

        // Parse parameters list.
        // While current token is not `)`.
        while (tk != ')') {
          // Set current parameter's data type.
          ty = INT;

          // If current parameter's data type is `int`, read token.
          if (tk == Int) next();
          // If current parameter's data type is `char`, read token, set
          // data type be `CHAR`.
          else if (tk == Char) { next(); ty = CHAR; }

          // While current token is `*`, it is pointer type.
          // Add `PTR` to the data type.
          while (tk == Mul) { next(); ty = ty + PTR; }

          // Current token should be parameter name.
          // If current token is not identifier, print error and exit program.
          if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; }

          // If the parameter name has been defined before as parameter, print
          // error and exit program.
          if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; }

          // Back up the symbol's `Class`, `Type`, `Val` fields because they
          // will be used temporarily for the parameter name.
          // Set the symbol type be local variable.
          // Set the associated value type be the parameter's data type.
          // Store the parameter's index.
          id[HClass] = id[Class]; id[Class] = Loc;
          id[HType]  = id[Type];  id[Type] = ty;
          id[HVal]   = id[Val];   id[Val] = i++;

          // Read token.
          next();

          // If current token is `,`, skip.
          if (tk == ',') next();
        }

        // Read token.
        next();

        // If current token is not function body's `{`, print error and exit
        // program.
        if (tk != '{') { printf("%d: bad function definition\n", line); return -1; }

        // Local variable offset.
        loc = ++i;

        // Read token.
        next();

        // While current token is `int` or `char`, it is variable definition.
        while (tk == Int || tk == Char) {
          // Set base data type.
          bt = (tk == Int) ? INT : CHAR;

          // Read token.
          next();

          // While statement end is not met.
          while (tk != ';') {
            // Set base data type.
            ty = bt;

            // While current token is `*`, it is pointer type.
            // Add `PTR` to the data type.
            while (tk == Mul) { next(); ty = ty + PTR; }

            // Current token should be local variable name.
            // If current token is not identifier, print error and exit
            // program.
            if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }

            // If the local variable name has been defined before as local
            // variable, print error and exit program.
            if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; }

            // Back up the symbol's `Class`, `Type`, `Val` fields because they
            // will be used temporarily for the local variable name.
            // Set the symbol type be local variable.
            // Set the associated value type be the local variable's data type.
            // Store the local variable's index.
            id[HClass] = id[Class]; id[Class] = Loc;
            id[HType]  = id[Type];  id[Type] = ty;
            id[HVal]   = id[Val];   id[Val] = ++i;

            // Read token.
            next();

            // If current token is `,`, skip.
            if (tk == ',') next();
          }

          // Read token.
          next();
        }

        // Add `ENT` instruction before function body.
        // Add local variables count as operand.
        *++e = ENT; *++e = i - loc;

        // While current token is not function body's ending `}`,
        // parse statement.
        while (tk != '}') stmt();

        // Add `LEV` instruction after function body.
        *++e = LEV;

        // Point `id` to symbol table.
        id = sym; // unwind symbol table locals

        // While current symbol table entry is in use.
        while (id[Tk]) {
          // If the symbol table entry is for function parameter or local
          // variable.
          if (id[Class] == Loc) {
            // Restore `Class`, `Type` and `Val` fields' old value.
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }

          // Point to next symbol table entry.
          id = id + Idsz;
        }
      }
      // If current token is not `(`, then it is not function definition,
      // assume it is global variable definition.
      else {
        // Set symbol type.
        id[Class] = Glo;

        // Store the global variable's address.
        id[Val] = (int)data;

        // Point to next global variable.
        data = data + sizeof(int);
      }

      // If current token is `,`, skip.
      if (tk == ',') next();
    }

    // Read token.
    next();
  }

  // Point instruction pointer `pc` to `main` function's address.
  // If symbol `main`'s `Val` field is not set, it means `main` function is
  // not defined, print error and exit program.
  if (!(pc = (int *)idmain[Val])) { printf("main() not defined\n"); return -1; }

  // If switch for printing source code line and corresponding instructions is
  // on, exit program.
  if (src) return 0;

  // setup stack
  // Point frame base pointer `bp` and stack top pointer `sp` to stack bottom.
  bp = sp = (int *)((int)sp + poolsz);

  // Push `EXIT` instruction to stack.
  // Note the stack grows towards lower address so after the `PSH` instruction
  // added below is executed, this `EXIT` instruction will be executed to exit
  // the program.
  *--sp = EXIT; // call exit if main returns

  // Push `PSH` instruction to stack to push exit code in register to stack
  // after `main` function returns. The exit code on stack will be used by the
  // `EXIT` instruction added above.
  // Point `t` to the `PSH` instruction's address.
  *--sp = PSH; t = sp;

  // Push `main` function's first argument `argc` to stack.
  *--sp = argc;

  // Push `main` function's second argument `argv` to stack.
  *--sp = (int)argv;

  // Push the `PSH` instruction's address to stack so that `main` function
  // will return to the `PSH` instruction.
  *--sp = (int)t;

  // run...
  // Instruction cycles count.
  cycle = 0;
  // Run VM loop to execute VM instructions.
  while (1) {
    // Get current instruction.
    // Increment instruction pointer.
    // Increment instruction cycles count.
    i = *pc++; ++cycle;

    // If debug switch is on.
    if (debug) {
      // Print opcode.
      printf("%d> %.4s", cycle,
        &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
         "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[i * 5]);

      // If the opcode <= ADJ, it has operand.
      // Print operand.
      if (i <= ADJ) printf(" %d\n", *pc); else printf("\n");
    }

    // Add the base address in frame base pointer `bp` to the offset in the
    // operand.
    if      (i == LEA) a = (int)(bp + *pc++);                             // load local address
    // Load the operand to register.
    else if (i == IMM) a = *pc++;                                         // load global address or immediate
    // Jump to the address in the operand.
    else if (i == JMP) pc = (int *)*pc;                                   // jump
    // Push the return address in the second operand to stack.
    // Jump to the address in the first operand.
    else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; }        // jump to subroutine
    // Jump to the address in the first operand if register value is 0.
    else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc;                      // branch if zero
    // Jump to the address in the first operand if register value is not 0.
    else if (i == BNZ) pc = a ? (int *)*pc : pc + 1;                      // branch if not zero
    // Push the caller's frame base address in `bp` to stack.
    // Point `bp` to stack top for the callee.
    // Decrease stack top pointer `sp` by the value in the operand to reserve
    // space for the callee's local variables.
    else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; }     // enter subroutine
    // Pop arguments off stack after returning from function call.
    else if (i == ADJ) sp = sp + *pc++;                                   // stack adjust
    // Point stack top pointer `sp` to caller's stack top before the call.
    // Pop caller's frame base address off stack into `bp`.
    // The old value was pushed to stack by `ENT` instruction.
    else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // leave subroutine
    // Load int value on the address in register to register.
    else if (i == LI)  a = *(int *)a;                                     // load int
    // Load char value on the address in register to register.
    else if (i == LC)  a = *(char *)a;                                    // load char
    // Save int value in register to address on stack.
    else if (i == SI)  *(int *)*sp++ = a;                                 // store int
    // Save char value in register to address on stack.
    else if (i == SC)  a = *(char *)*sp++ = a;                            // store char
    // Push register value to stack.
    else if (i == PSH) *--sp = a;                                         // push

    // The following instructions take two arguments.
    // The first argument is on stack.
    // The second argument is in register.
    // The result is put to register.
    else if (i == OR)  a = *sp++ |  a;
    else if (i == XOR) a = *sp++ ^  a;
    else if (i == AND) a = *sp++ &  a;
    else if (i == EQ)  a = *sp++ == a;
    else if (i == NE)  a = *sp++ != a;
    else if (i == LT)  a = *sp++ <  a;
    else if (i == GT)  a = *sp++ >  a;
    else if (i == LE)  a = *sp++ <= a;
    else if (i == GE)  a = *sp++ >= a;
    else if (i == SHL) a = *sp++ << a;
    else if (i == SHR) a = *sp++ >> a;
    else if (i == ADD) a = *sp++ +  a;
    else if (i == SUB) a = *sp++ -  a;
    else if (i == MUL) a = *sp++ *  a;
    else if (i == DIV) a = *sp++ /  a;
    else if (i == MOD) a = *sp++ %  a;

    // The following instructions are system calls.
    // They take arguments from stack, just like a user-defined function does.
    // Note the stack grows towards lower address, arguments pushed earlier are
    // at higher address. E.g. if there are three arguments on stack, then:
    // `sp[2]` is the first argument.
    // `sp[1]` is the second argument.
    // `*sp` is the third argument.
    //
    // Open file.
    // Arg 1: The file path to open.
    // Arg 2: The flags.
    else if (i == OPEN) a = open((char *)sp[1], *sp);
    // Read from file descriptor into buffer.
    // Arg 1: The file descriptor.
    // Arg 2: The buffer pointer.
    // Arg 3: The number of bytes to read.
    else if (i == READ) a = read(sp[2], (char *)sp[1], *sp);
    // Close file descriptor.
    // Arg 1: The file descriptor.
    else if (i == CLOS) a = close(*sp);
    // Print formatted string.
    // Because the call has arguments, an ADJ instruction should have been
    // added. `pc[1]` gets the ADJ instruction's operand, i.e. the number of
    // arguments.
    // Arg 1: The format string.
    // Arg 2-7: The formatted values.
    else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); }
    // Allocate memory block.
    // Arg 1: The number of bytes to allocate.
    else if (i == MALC) a = (int)malloc(*sp);
    // Free memory block allocated.
    // Arg 1: The memory block pointer.
    else if (i == FREE) free((void *)*sp);
    // Set every byte in a memory buffer to the same value.
    // Arg 1: The buffer pointer.
    // Arg 2: The value.
    // Arg 3: The number of bytes to set.
    else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp);
    // Compare memory buffer.
    // Arg 1: The first buffer pointer.
    // Arg 2: The second buffer pointer.
    // Arg 3: The number of bytes to compare.
    else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp);
    // Exit program.
    // Arg 1: The exit code.
    else if (i == EXIT) { printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp; }
    // Current instruction is unknown, print error and exit program.
    else { printf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; }
  }
}
