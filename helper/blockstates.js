const mcData = require('minecraft-data')('1.16.5')

for (const name of [
    'stone',
    'glass',
    'purple_stained_glass',
    'dirt',
    'grass_block',
    'oak_planks'
]) {
    const block = mcData.blocksByName[name]
    console.log(name, {
        defaultState: block.defaultState,
        states: block.states
    })
}