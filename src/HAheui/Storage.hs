module HAheui.Storage(
    Storage, emptyStack, emptyQueue,
    push, pushFront, front, pop,
    Storages,
    createStorages
) where

data Storage = Stack {
    rawData :: [Integer]
} | Queue {
    rawData :: [Integer]
}
emptyStack = Stack []
emptyQueue = Queue []

push :: Storage -> Integer -> Storage
push (Stack stack) value = Stack (value:stack)
push (Queue queue) value = Queue (queue ++ [value])

pushFront :: Storage -> Integer -> Storage
pushFront (Queue queue) value = Queue (value:queue)
pushFront storage value = push storage value

front :: Storage -> Integer
front (Stack stack) = head stack
front (Queue queue) = head queue

pop :: Storage -> Storage
pop (Stack stack) = Stack (tail stack)
pop (Queue queue) = Queue (tail queue)

type Storages = [Storage]
createStorages = [ emptyStack, emptyStack, emptyStack, emptyStack, emptyStack, emptyStack, emptyStack,
                   emptyStack, emptyStack, emptyStack, emptyStack, emptyStack, emptyStack, emptyStack,
                   emptyStack, emptyStack, emptyStack, emptyStack, emptyStack, emptyStack, emptyStack,
                   emptyQueue, emptyStack, emptyStack, emptyStack, emptyStack, emptyStack, emptyStack ]