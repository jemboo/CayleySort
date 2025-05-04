module ResultTests

open Xunit
open FsUnit.Xunit

// Types
type Username = string
type Email = string
type User = { Username: Username; Email: Email }
type ValidationError = string

// Mock async database operations
let checkUsernameUnique (username: Username) : AsyncResult<bool, ValidationError> =
    async {
        // Simulate async database query
        do! Async.Sleep 100
        if username = "taken" then
            return Error "Username is already taken"
        else
            return Ok true
    }

let validateEmail (email: Email) : Result<bool, ValidationError> =
    if email.Contains "@" then Ok true
    else Error "Invalid email format"

let saveUser (user: User) : AsyncResult<User, ValidationError> =
    async {
        // Simulate async database save
        do! Async.Sleep 100
        return Ok user
    }

// Validate and save user
let createUser (username: Username) (email: Email) : AsyncResult<User, ValidationError> =
    AsyncResultComputationExpression.asyncResult {
        // Validate username uniqueness (async)
        let! isUsernameUnique = checkUsernameUnique username
        
        // Validate email (sync, lifted to AsyncResult)
        let! isEmailValid = validateEmail email |> AsyncResult.ofResult
        
        // Ensure both validations passed
        if isUsernameUnique && isEmailValid then
            let user = { Username = username; Email = email }
            // Save user (async)
            let! savedUser = saveUser user
            return savedUser
        else
            return! AsyncResult.ofError "Validation failed"
    }



[<Fact>]
let ``createUser succeeds with valid input`` () =
    async {
        let! result = createUser "newuser" "test@example.com"
        match result with
        | Ok user ->
            user.Username |> should equal "newuser"
            user.Email |> should equal "test@example.com"
        | Error err -> failwith $"Expected Ok, got Error: {err}"
    }

[<Fact>]
let ``createUser fails with taken username`` () =
    async {
        let! result = createUser "taken" "test@example.com"
        match result with
        | Error err -> err |> should equal "Username is already taken"
        | Ok _ -> failwith "Expected Error, got Ok"
    }

[<Fact>]
let ``createUser fails with invalid email`` () =
    async {
        let! result = createUser "newuser" "invalid-email"
        match result with
        | Error err -> err |> should equal "Invalid email format"
        | Ok _ -> failwith "Expected Error, got Ok"
    }