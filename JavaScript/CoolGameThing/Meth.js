/**
 * Gets the remainder of two numbers, up to precision decimal places of precision.
 *
 * Will fail if dividend is not a number.
 * precision and divisor will be set to 1 if they are not numbers.
 * percision will be set to 1 if it is less than or equal to 0.
 * divisor will be set to 1 if it is equal to 0.
 *
 * @param {number} dividend The dividend of the division.
 * @param {number} divisor The divisor of the division.
 * @param {number} precision The amount of decimal places of precision.
 *
 * @returns dividend % divisor with precision decimal places of precision.
 */
export function fmod(dividend, divisor, precision) {
    if (typeof dividend !== 'number')
        return;
    
    if (typeof precision !== 'number' || precision <= 0)
        precision = 1;
    
    if (typeof divisor !== 'number' || divisor === 0)
        divisor = 1;
    
    precision = 10 ** precision;
    
    return (truncateUnsafely(dividend * precision) % truncateUnsafely(divisor * precision)) / precision;
}

/**
 * Gets the remainder of two numbers, up to precision decimal places of precision.
 *
 * percision will be set to 1 if it is less than or equal to 0.
 * divisor will be set to 1 if it is equal to 0.
 * WARNING!: does not account for faulty input of any kind.
 *
 * @param {number} dividend The dividend of the division.
 * @param {number} divisor The divisor of the division.
 * @param {number} precision The amount of decimal places of precision.
 *
 * @returns dividend % divisor with precision decimal places of precision.
 */
export function fmodUnsafe(dividend, divisor, precision) {
    if (precision <= 0)
        precision = 1;
    
    if (divisor === 0)
        divisor = 1;
    
    precision = 10 ** precision;
    
    return (truncateUnsafely(dividend * precision) % truncateUnsafely(divisor * precision)) / precision;
}

/**
 * Truncates a float into a integer.
 *
 * Fails if number is not a number.
 *
 * @param {number} number The number to truncate.
 *
 * @returns The truncated form of number
 */
export function truncate(number) {
    if (typeof number !== 'number')
        return;
    
    if (number > 0) {
        return Math.floor(number);
    
    } else
        return Math.ceil(number);
}

/**
 * Truncates a float into a integer.
 *
 * WARNING!: does not account for faulty input of any kind.
 *
 * @param {number} number The number to truncate.
 *
 * @returns The truncated form of number
 */
export function truncateUnsafely(number) {
    if (number > 0) {
        return Math.floor(number);
    
    } else
        return Math.ceil(number);
}