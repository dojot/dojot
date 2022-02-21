/**
 * Service to handle Devices
 */
 class DeviceService {
    /**
     * The dependencies are injected through the constructor
     */
    constructor({
      ejbcaFacade, rootCA, pkiUtils,
    }) {
      Object.defineProperty(this, 'ejbcaFacade', { value: ejbcaFacade });
      Object.defineProperty(this, 'rootCA', { value: rootCA });
      Object.defineProperty(this, 'pkiUtils', { value: pkiUtils });
    }

    async createDevices(label, templates) {
        const caPem = await this.ejbcaFacade.getRootCertificate(this.rootCA);
    
        const certificateFingerprint = this.pkiUtils.getFingerprint(caPem);
    
        return { certificateFingerprint, caPem };
      }
}