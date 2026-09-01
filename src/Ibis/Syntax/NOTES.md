# Notes

## 1. Surface Hole Discovery
- Identify all typed holes (metavariables) in the surface syntax and assign them unique identifiers eg `?m42`.
- Capture the local context: Record every bound variable, its type, and its visibility in scope at the exact location where ?m42 was instantiated. ?m42 is not just a hole; it is a higher-order term applied to its local environment: ?m42(x_1, x_2, ..., x_n).

## 2. Constraint Generation
- Traverse the AST using bidirectional type checking (inference and checking)
- Whenever two types must be definitionally equal, generate a unification constraint (e.g matching a function argument type with an applied value type).
- If `?m42` appears in the equation, store the equation in the constraint set for later resolution.

## 3. Constraint Resolution (Pattern Unification)
- Process the constraint set using Miller Pattern Unification
    - **Pattern Check**: Make sure the arguments applied to the metavariable are distinct bound variables (no duplicates, no free variables).
    - **Scope/Occurs check**: Ensure that the metavariable does not appear in itself (preventing infinite terms), and only uses variables from its local context.
    - **Solution**: If the checks pass, solve the metavariable by substituting it with a lambda abstraction over its local context, applied to the right-hand side of the equation.

## 4. Zonking
- Maintain a global metavariable substitution map that records the solutions for each metavariable.
- Perform a total pass over the elaborated AST, replacing every occurrence of a metavariable with its solution from the substitution map. This is called
zonking, and it ensures that the final term is fully elaborated with no remaining metavariables.