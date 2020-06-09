const { ConditionApplier } = require('../../app/Conditions');
jest.mock('@dojot/microservice-sdk');

describe('Testing Conditions', () => {
    beforeAll(() => {
    });
    beforeEach(() => {
        jest.clearAllMocks();
    });


    it('in with data', () => {

        const filter = ConditionApplier('sensor.status', 'in', ['failed', 'stopped']);
        const object = { sensor: { status: 'failed' }, temperature: 35 };
        const filterData = filter(object);

        expect(filterData).toMatchObject({ sensor: { status: 'failed' }, temperature: 35 });

    });

    it('in with no data', () => {

        const filter = ConditionApplier('sensor.status', 'in', ['failed', 'stopped']);
        const object = { sensor: { status: 'started' }, temperature: 35 };
        const filterData = filter(object);

        expect(filterData).toMatchObject({});

    });

    it('nin with data', () => {

        const filter = ConditionApplier('sensor.status', 'nin', ['failed', 'stopped']);
        const object = { sensor: { status: 'failed' }, temperature: 35 };
        const filterData = filter(object);

        expect(filterData).toMatchObject({});

    });

    it('nin with no data', () => {

        const filter = ConditionApplier('sensor.status', 'nin', ['failed', 'stopped']);
        const object = { sensor: { status: 'started' }, temperature: 35 };
        const filterData = filter(object);

        expect(filterData).toMatchObject({ sensor: { status: 'started' }, temperature: 35 });

    });

    it('gte with data ', () => {

        const filter = ConditionApplier('temperature', 'gte', [20]);
        const object = { location: 'x', temperature: 20 };
        const filterData = filter(object);

        expect(filterData).toMatchObject({ location: 'x' });

    });

    it('gte with no data ', () => {

        const filter = ConditionApplier('temperature', 'gte', [21]);
        const object = { location: 'x', temperature: 20 };
        const filterData = filter(object);

        expect(filterData).toMatchObject({});

    });

    it('gt with data', () => {

        const filter = ConditionApplier('temperature', 'gt', [20]);
        const object = { location: 'x', temperature: 21 };
        const filterData = filter(object);

        expect(filterData).toMatchObject({ location: 'x' });

    });

    it('gt with no data', () => {

        const filter = ConditionApplier('temperature', 'gt', [21]);
        const object = { location: 'x', temperature: 21 };
        const filterData = filter(object);

        expect(filterData).toMatchObject({});

    });



    it('lt with data', () => {

        const filter = ConditionApplier('temperature', 'lt', [20]);
        const object = { location: 'x', temperature: 19 };
        const filterData = filter(object);

        expect(filterData).toMatchObject({ location: 'x' });

    });

    it('lt with no data', () => {

        const filter = ConditionApplier('temperature', 'lt', [20]);
        const object = { location: 'x', temperature: 21 };
        const filterData = filter(object);

        expect(filterData).toMatchObject({});

    });

    it('lte with data', () => {

        const filter = ConditionApplier('temperature', 'lte', [19]);
        const object = { location: 'x', temperature: 19 };
        const filterData = filter(object);

        expect(filterData).toMatchObject({ location: 'x' });

    });

    it('lte with no data', () => {

        const filter = ConditionApplier('temperature', 'lte', [20]);
        const object = { location: 'x', temperature: 21 };
        const filterData = filter(object);

        expect(filterData).toMatchObject({});

    });

    it('eq with data', () => {

        const filter = ConditionApplier('temperature', 'eq', [19]);
        const object = { location: 'x', temperature: 19 };
        const filterData = filter(object);

        expect(filterData).toMatchObject({ location: 'x' });

    });

    it('eq with no data', () => {

        const filter = ConditionApplier('temperature', 'eq', [20]);
        const object = { location: 'x', temperature: 21 };
        const filterData = filter(object);

        expect(filterData).toMatchObject({});

    });

    it('neq with data', () => {

        const filter = ConditionApplier('temperature', 'neq', [20]);
        const object = { location: 'x', temperature: 19 };
        const filterData = filter(object);

        expect(filterData).toMatchObject({ location: 'x' });

    });

    it('neq with no data', () => {

        const filter = ConditionApplier('temperature', 'neq', [20]);
        const object = { location: 'x', temperature: 20 };
        const filterData = filter(object);

        expect(filterData).toMatchObject({});

    });

});